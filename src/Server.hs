{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Server where

import Control.Monad
import Control.Monad.Random
import Control.Monad.State.Strict
import Data.Maybe

import Network.UDP.Pal hiding (newClientThread)

import Control.Concurrent
import Control.Concurrent.STM

import Control.Lens.Extra

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import qualified Data.Sequence as Seq
import Data.Sequence (Seq)

import Data.Foldable

import Physics.Bullet
import Types
import Linear.Extra

handRigidBodyID :: RigidBodyID
handRigidBodyID = RigidBodyID 1

data ServerState = ServerState 
  { _ssRigidBodies         :: !(Map ObjectID RigidBody)
  , _ssPlayerRigidBodies   :: !(Map PlayerID [RigidBody])
  , _ssPlayers             :: !(Map PlayerID Player)
  , _ssPlayerIDs           :: !(Map SockAddr PlayerID)
  , _ssCubeExpirationQueue :: !(Seq ObjectID)
  , _ssCenterCubeID        :: !(Maybe ObjectID)
  }

newServerState :: ServerState
newServerState = ServerState mempty mempty mempty mempty mempty Nothing

makeLenses ''ServerState

objIDFromRigidBodyID :: RigidBodyID -> ObjectID
objIDFromRigidBodyID = fromIntegral . unRigidBodyID
rigidBodyIDFromObjID :: ObjectID -> RigidBodyID
rigidBodyIDFromObjID = RigidBodyID . fromIntegral

beginToSpawnNextCube :: (MonadIO m, MonadState ServerState m) => TChan () -> m ()
beginToSpawnNextCube spawnEvents = do
  ssCenterCubeID .= Nothing

  -- Spawn a maximum of 16 cubes
  numberOfCubes <- Seq.length <$> use ssCubeExpirationQueue
  when (numberOfCubes < 16) $ 
    void . liftIO . forkIO $ do
      threadDelay (1000000 * 2)
      atomically (writeTChan spawnEvents ())

spawnNextCube :: (MonadIO m, MonadState ServerState m, MonadRandom m) 
              => Server Op -> DynamicsWorld -> m ()
spawnNextCube Server{..} dynamicsWorld = do
  
  -- Spawn a cube at the center
  objID <- getRandom
  
  let instruction = CreateObject objID (Object newPose cubeScale)
  ssCenterCubeID .= Just objID

  interpretS dynamicsWorld svrSockAddr instruction
  liftIO . svrBroadcast $ Reliable instruction

physicsServer :: IO ()
physicsServer = do

  -- This is just a dummy value to pass to interpretS when calling with a message we generated
  serverName <- findLocalIP
  writeFile "serverIP.txt" serverName

  server@Server{..} <- createServer serverName serverPort packetSize
  putStrLn $ "Server engaged on " ++ serverName

  spawnEvents <- newTChanIO
  
  -- Initialize physics
  dynamicsWorld  <- createDynamicsWorld mempty { gravity = 0.0 }
  _              <- addStaticRoom dynamicsWorld (RigidBodyID 0) (-roomScale)
  
  void . flip runStateT newServerState $ do
    spawnNextCube server dynamicsWorld
    forever $ serverLoop server dynamicsWorld spawnEvents

serverLoop :: (MonadIO m, MonadState ServerState m, MonadRandom m) 
           => Server Op -> DynamicsWorld -> TChan () -> m ()
serverLoop server@Server{..} dynamicsWorld spawnEvents = do
  handleDisconnections server dynamicsWorld
  -- Receive updates from clients
  interpretNetworkPacketsFromOthers (liftIO svrReceive) $ \fromAddr message -> 
    -- Apply to server-only state
    interpretS dynamicsWorld fromAddr message
  
  -- Check if we should spawn a new cube
  (liftIO . atomically . tryReadTChan) spawnEvents >>= \case
    Nothing -> return ()
    Just () -> spawnNextCube server dynamicsWorld

  -- Run the physics sim
  stepSimulation dynamicsWorld
  
  -- Find collisions so we can transmit them to clients      
  collisions <- getCollisions dynamicsWorld

  -- Generate a list of instructions updating 
  -- each object with the state from the physics sim
  players     <- use (ssPlayers     . to Map.toList)
  rigidBodies <- use (ssRigidBodies . to Map.toList)
  
  collisionUpdates <- fmap catMaybes . forM collisions $ \collision -> do
    -- Must grab this in the loop, as we want the first collision
    -- that successfully grabs it to overwrite it with Nothing
    centerCubeID <- use ssCenterCubeID
    let objAID = objIDFromRigidBodyID (cbBodyAID collision)
        objBID = objIDFromRigidBodyID (cbBodyBID collision)
        strength = cbAppliedImpulse collision
        oneBodyIsCenter = Just objAID         == centerCubeID    || Just objBID         == centerCubeID
        -- oneBodyIsHand   = cbBodyAID collision == handRigidBodyID || cbBodyAID collision == handRigidBodyID
    -- when (oneBodyIsCenter && oneBodyIsHand) $ 
    when oneBodyIsCenter $
      beginToSpawnNextCube spawnEvents

    return $! if strength > 0.05
      then Just (ObjectCollision objAID objBID strength)
      else Nothing
  playerUpdates <- forM players $ \(playerID, player) -> 
    return $! UpdatePlayer playerID player
  objectUpdates <- forM rigidBodies $ \(objID, rigidBody) -> do
    (pos, orient) <- getBodyState rigidBody

    return $! UpdateObject objID (Object (Pose pos orient) cubeScale)
  let transientInstructions = playerUpdates ++ objectUpdates ++ collisionUpdates

  -- Broadcast the simulation results to all clients
  liftIO . svrBroadcast $ Unreliable transientInstructions


  -- Generate reliable instructions
  -- Add the cube to the queue, and delete the oldest cubes
  deleters <- Seq.drop maxCubes <$> use ssCubeExpirationQueue

  let reliableInstructions = DeleteObject <$> toList deleters

  forM_ reliableInstructions $ \instruction -> do
    interpretS dynamicsWorld svrSockAddr instruction
    liftIO . svrBroadcast $ Reliable instruction

  -- Run at 60 FPS
  liftIO $ threadDelay (1000000 `div` 60)

-- | Interpret a client message creating an object
-- to add it to the physics world
interpretS :: (MonadIO m, MonadState ServerState m) 
           => DynamicsWorld -> SockAddr -> Op -> m ()
interpretS dynamicsWorld _fromAddr (CreateObject objID obj) = do
  
  rigidBody <- addCube dynamicsWorld (rigidBodyIDFromObjID objID)
                       mempty { pcPosition = obj ^. objPose  . posPosition
                              , pcRotation = obj ^. objPose  . posOrientation
                              , pcScale    = obj ^. objScale . to realToFrac
                              }
  
  -- Shoot the cube outwards
  --let v = rotate ( obj ^. objPose . posOrientation ) ( V3 0 0 ( -3 ) )
  --_ <- applyCentralForce rigidBody v

  ssRigidBodies . at objID ?= rigidBody

  -- Add the cube to the expiration queue
  ssCubeExpirationQueue %= (objID <|)

-- Update our record of the player's body/head/hands positions, 
-- and update the rigid bodies of their hands
interpretS dynamicsWorld _fromAddr (UpdatePlayer playerID player) = do
  ssPlayers . at playerID ?= player

  maybeHandRigidBodies <- use $ ssPlayerRigidBodies . at playerID

  -- Check if we've created rigid bodies for the hands yet.
  -- We do this in Update to support "late hands", e.g. plugging
  -- in the hydra after the executable is running
  case (maybeHandRigidBodies, player ^. plrHandPoses) of
    (Nothing, hands) | not (null hands) -> do
      handRigidBodies <- forM (player ^. plrHandPoses) $ \_ -> do
        body <- addCube dynamicsWorld handRigidBodyID
                        mempty { pcScale = handDimensions
                               }
        setRigidBodyKinematic body
        return body
      ssPlayerRigidBodies . at playerID ?= handRigidBodies
    _ -> return ()

  maybeHandRigidBodies' <- use $ ssPlayerRigidBodies . at playerID
  forM_ maybeHandRigidBodies' $ \handRigidBodies -> 
    forM_ (zip (player ^. plrHandPoses) handRigidBodies) $ \(handPose, handRigidBody) -> do
      let Pose handPosition handOrientation = shiftBy handOffset handPose
      setRigidBodyWorldTransform handRigidBody handPosition handOrientation

interpretS _dynamicsWorld fromAddr (Connect playerID player) = do
  -- Associate the playerID with the fromAddr we already know,
  -- so we can send an accurate disconnect message later
  ssPlayerIDs . at fromAddr ?= playerID
  ssPlayers   . at playerID ?= player
  

interpretS dynamicsWorld fromAddr (Disconnect playerID) = do
  ssPlayerIDs . at fromAddr .= Nothing

  rigidBodies <- use $ ssPlayerRigidBodies . at playerID
  maybe (return ()) (mapM_ (removeCube dynamicsWorld)) rigidBodies
  ssPlayerRigidBodies . at playerID .= Nothing

interpretS dynamicsWorld _fromAddr (DeleteObject objID) = do

  rigidBody <- use $ ssRigidBodies . at objID
  maybe (return ()) (removeCube dynamicsWorld) rigidBody
  
  ssCubeExpirationQueue    %= Seq.filter (/= objID)
  ssRigidBodies . at objID .= Nothing

interpretS _dynamicsWorld _fromAddr (UpdateObject _ _) = return ()
interpretS _ _ (ObjectCollision _ _ _) = return ()


handleDisconnections :: (MonadIO m, MonadState ServerState m) 
                     => Server Op -> DynamicsWorld -> m ()
handleDisconnections Server{..} dynamicsWorld = do
  disconnections <- liftIO svrGetDisconnects
  forM_ disconnections $ \fromAddr -> do
    -- For each SockAddr we've detected a disconnection from,
    -- find its associated player ID and broadcast a message to clients
    -- informing them that the player has left so they can clear any
    -- visible rendering of that player.

    maybePlayerID <- use $ ssPlayerIDs . at fromAddr
    case maybePlayerID of
      Nothing -> putStrLnIO $ "Couldn't find playerID for disconnecting address: " ++ show fromAddr
      Just playerID -> do
        let message = Disconnect playerID

        interpretS dynamicsWorld fromAddr message
        
        liftIO . svrBroadcast $ (Reliable message)

        putStrLnIO $ "Goodbye: " ++ show fromAddr
