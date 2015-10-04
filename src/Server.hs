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
  }

newServerState :: ServerState
newServerState = ServerState mempty mempty mempty mempty mempty

makeLenses ''ServerState



physicsServer :: IO ()
physicsServer = do

  -- This is just a dummy value to pass to interpretS when calling with a message we generated
  serverName <- findLocalIP
  writeFile "serverIP.txt" serverName

  server@Server{..} <- createServer serverName serverPort packetSize
  putStrLn $ "Server engaged on " ++ serverName
  
  -- Initialize physics
  dynamicsWorld  <- createDynamicsWorld mempty { gravity = 0.0 }
  _              <- addStaticRoom dynamicsWorld (RigidBodyID 0) (-roomScale)
  
  void . flip runStateT newServerState $ 
    forever $ serverLoop server dynamicsWorld

serverLoop :: (MonadIO m, MonadState ServerState m, MonadRandom m) 
           => Server Op -> DynamicsWorld -> m ()
serverLoop server@Server{..} dynamicsWorld = do
  handleDisconnections server dynamicsWorld
  -- Receive updates from clients
  interpretNetworkPacketsFromOthers (liftIO svrReceive) $ \fromAddr message -> 
    -- Apply to server-only state
    interpretS server dynamicsWorld fromAddr message

  -- Run the physics sim
  stepSimulation dynamicsWorld
  
  -- Find collisions so we can transmit them to clients      
  collisions <- getCollisions dynamicsWorld

  -- Generate a list of instructions updating 
  -- each object with the state from the physics sim
  
  
  collisionUpdates <- fmap catMaybes . forM collisions $ \collision -> do
    return $! if cbAppliedImpulse collision > 0.05
      then Just (ObjectCollision collision)
      else Nothing


  players     <- use (ssPlayers     . to Map.toList)
  playerUpdates <- forM players $ \(playerID, player) -> 
    return $! UpdatePlayer playerID player

  rigidBodies <- use (ssRigidBodies . to Map.toList)
  objectUpdates <- forM rigidBodies $ \(objID, rigidBody) -> do
    (pos, orient) <- getBodyState rigidBody

    -- During the rigidBody loop, also apply vacuum forces if needed
    forM_ players $ \(_playerID, player) -> do
      when (player ^. plrVacuum) $ do
        let target = player ^. plrPose . posPosition
            -- orientation = normalize (target - pos)
            orientation = (target - pos) * 0.01
        setRigidBodyActive rigidBody
        _ <- applyCentralImpulse rigidBody orientation
        return ()

    return $! UpdateObject objID (Object (Pose pos orient) cubeScale)
  let transientInstructions = playerUpdates ++ objectUpdates ++ collisionUpdates

  -- Broadcast the simulation results to all clients
  liftIO . svrBroadcast $ Unreliable transientInstructions


  -- Generate reliable instructions
  -- Add the cube to the queue, and delete the oldest cubes
  deleters <- Seq.drop maxCubes <$> use ssCubeExpirationQueue

  let reliableInstructions = DeleteObject <$> toList deleters

  forM_ reliableInstructions $ \instruction -> do
    interpretS server dynamicsWorld svrSockAddr instruction
    liftIO . svrBroadcast $ Reliable instruction

  -- Run at 60 FPS
  liftIO $ threadDelay (1000000 `div` 60)



-- | Interpret a client message creating an object
-- to add it to the physics world
interpretS :: (MonadIO m, MonadState ServerState m) 
           => Server Op -> DynamicsWorld -> SockAddr -> Op -> m ()
interpretS _ dynamicsWorld _fromAddr (CreateObject objID obj) = do
  
  rigidBody <- addCube dynamicsWorld (fromIntegral objID)
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
interpretS _ dynamicsWorld _fromAddr (UpdatePlayer playerID player) = do
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

interpretS _ _dynamicsWorld fromAddr (Connect playerID player) = do
  -- Associate the playerID with the fromAddr we already know,
  -- so we can send an accurate disconnect message later
  ssPlayerIDs . at fromAddr ?= playerID
  ssPlayers   . at playerID ?= player
  

interpretS _ dynamicsWorld fromAddr (Disconnect playerID) = do
  ssPlayerIDs . at fromAddr .= Nothing

  rigidBodies <- use $ ssPlayerRigidBodies . at playerID
  maybe (return ()) (mapM_ (removeCube dynamicsWorld)) rigidBodies
  ssPlayerRigidBodies . at playerID .= Nothing

interpretS _ dynamicsWorld _fromAddr (DeleteObject objID) = do

  rigidBody <- use $ ssRigidBodies . at objID
  maybe (return ()) (removeCube dynamicsWorld) rigidBody
  
  ssCubeExpirationQueue    %= Seq.filter (/= objID)
  ssRigidBodies . at objID .= Nothing

interpretS _ _dynamicsWorld _fromAddr (UpdateObject _ _) = return ()
interpretS _ _ _ (ObjectCollision _) = return ()
interpretS server@Server{..} dynamicsWorld _ (Restart) = do
  deleters <- use ssCubeExpirationQueue
  let reliableInstructions = DeleteObject <$> toList deleters

  forM_ reliableInstructions $ \instruction -> do
    interpretS server dynamicsWorld svrSockAddr instruction
    liftIO . svrBroadcast $ Reliable instruction
  return ()



handleDisconnections :: (MonadIO m, MonadState ServerState m) 
                     => Server Op -> DynamicsWorld -> m ()
handleDisconnections server@Server{..} dynamicsWorld = do
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

        interpretS server dynamicsWorld fromAddr message
        
        liftIO . svrBroadcast $ (Reliable message)

        putStrLnIO $ "Goodbye: " ++ show fromAddr
