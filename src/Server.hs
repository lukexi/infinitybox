{-# LANGUAGE CPP #-}
#ifdef mingw32_HOST_OS
{-# OPTIONS_GHC -F -pgmF strip-ths #-}
#endif

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module Server where

import Control.Monad
import Control.Monad.State.Strict

import Network.UDP.Pal hiding (newClientThread)

import Control.Concurrent
import Control.Concurrent.STM
import Linear

import Control.Lens

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict (IntMap)

import qualified Data.Sequence as Seq
import Data.Sequence (Seq)

import Data.Foldable

import Physics.Bullet
import Physics
import Resources
import Types
import Game.Pal


-- | The maximum number of cubes before we start kicking cubes out
maxCubes :: Int
maxCubes = 16

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
  putStrLn "Server engaged..."

  -- This is just a dummy value to pass to interpretS when calling with a message we generated
  let serverFromAddr = SockAddrUnix serverName

  (getPacketsFromClients, broadcastToClients, disconnectionsChan) <- createServer serverName serverPort packetSize

  
  -- Initialize physics
  dynamicsWorld  <- createDynamicsWorld mempty { gravity = 0.0 }
  _              <- addBox dynamicsWorld (-1.5)
  
  void . flip runStateT newServerState . forever $ do
    handleDisconnections disconnectionsChan broadcastToClients dynamicsWorld
    -- Receive updates from clients
    interpretNetworkPacketsFromOthers getPacketsFromClients $ \fromAddr message -> 
      -- Apply to server-only state
      interpretS dynamicsWorld fromAddr message

    -- Run the physics sim
    stepSimulation dynamicsWorld
    
    -- Generate a list of instructions updating 
    -- each object with the state from the physics sim
    rigidBodies <- use (ssRigidBodies . to Map.toList)
    players     <- use (ssPlayers     . to Map.toList)
    
    transientInstructions <- (++) 
      <$> forM players (\(playerID, player) -> 
            return $! UpdatePlayer playerID player)
      <*> forM rigidBodies (\(objID, rigidBody) -> do
            (pos, orient) <- getBodyState rigidBody
            let scale' = 1.0
            return $! UpdateObject objID (Object (Pose pos orient) scale'))

    -- Broadcast the simulation results to all clients
    broadcastToClients $ Unreliable transientInstructions


    -- Generate reliable instructions
    -- Add the cube to the queue, and delete the oldest cubes
    deleters <- Seq.drop maxCubes <$> use ssCubeExpirationQueue

    let reliableInstructions = DeleteObject <$> toList deleters

    forM_ reliableInstructions $ \instruction -> do
      interpretS dynamicsWorld serverFromAddr instruction
      broadcastToClients $ Reliable instruction

    -- Run at 60 FPS
    liftIO $ threadDelay (1000000 `div` 60)



-- | Interpret a client message creating an object
-- to add it to the physics world
interpretS :: (MonadIO m, MonadState ServerState m) 
           => DynamicsWorld -> SockAddr -> Op -> m ()
interpretS dynamicsWorld _fromAddr (CreateObject objID obj) = do
  
  rigidBody <- addCube dynamicsWorld
                       mempty { position = obj ^. objPose  . posPosition
                              , rotation = obj ^. objPose  . posOrientation
                              , scale    = obj ^. objScale . to realToFrac
                              }
  
  -- Shoot the cube outwards
  let v = rotate ( obj ^. objPose . posOrientation ) ( V3 0 0 ( -3 ) )
  _ <- applyCentralForce rigidBody v
  ssRigidBodies . at objID ?== rigidBody

  -- Add the cube to the expiration queue
  ssCubeExpirationQueue %== (objID <|)

-- Update our record of the player's body/head/hands positions, 
-- and update the rigid bodies of their hands
interpretS _dynamicsWorld _fromAddr (UpdatePlayer playerID player) = do
  ssPlayers . at playerID ?== player

  maybeHandRigidBodies <- use $ ssPlayerRigidBodies . at playerID
  forM_ maybeHandRigidBodies $ \handRigidBodies -> 
    forM_ (zip (player ^. plrHandPoses) handRigidBodies) $ \(handPose, handRigidBody) -> 
      setRigidBodyWorldTransform handRigidBody (handPose ^. posPosition) (handPose ^. posOrientation)

interpretS dynamicsWorld fromAddr (Connect playerID) = do
  -- Associate the playerID with the fromAddr we already know,
  -- so we can send an accurate disconnect message later
  ssPlayerIDs . at fromAddr ?== playerID

  -- Add rigid bodies for the player's hands that we'll
  -- update with their positions on receipt later
  handRigidBodies <- replicateM 2 $ do
    body <- addCube dynamicsWorld
                    mempty { scale = handDimensions
                           }
    setRigidBodyKinematic body
    return body

  ssPlayerRigidBodies . at playerID ?== handRigidBodies

interpretS _dynamicsWorld fromAddr (Disconnect _) = 
  ssPlayerIDs . at fromAddr .== Nothing

interpretS dynamicsWorld _fromAddr (DeleteObject objID) = do

  rigidBody <- use $ ssRigidBodies . at objID
  maybe (return ()) (removeCube dynamicsWorld) rigidBody
  
  ssCubeExpirationQueue    %== Seq.filter (/= objID)
  ssRigidBodies . at objID .== Nothing

interpretS _dynamicsWorld _fromAddr (UpdateObject _ _) = return ()



handleDisconnections :: (MonadIO m, MonadState ServerState m) 
                     => TChan SockAddr -> (AppPacket Op -> m ()) -> DynamicsWorld -> m ()
handleDisconnections disconnectionsChan broadcastToClients dynamicsWorld = do
  disconnections <- liftIO (atomically (exhaustChan disconnectionsChan))
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
        
        broadcastToClients (Reliable message)

        putStrLnIO $ "Goodbye: " ++ show fromAddr
