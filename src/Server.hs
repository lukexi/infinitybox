{-# OPTIONS_GHC -F -pgmF strip-ths #-}

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

  (getPacketsFromClients, broadcastToClients, disconnectionsChan) <- createServer serverName serverPort packetSize

  
  -- Initialize physics
  dynamicsWorld  <- createDynamicsWorld mempty { gravity = 0.0 }
  _              <- addBox dynamicsWorld (-1.5)
  
  
  let world = newWorld "Server"
  void . flip runStateT newServerState . flip runStateT world . forever $ do
    handleDisconnections disconnectionsChan broadcastToClients dynamicsWorld
    -- Receive updates from clients
    interpredNetworkPacketsFromOthers getPacketsFromClients $ \fromAddr message -> do
      -- Apply to our state
      interpret message
      -- Apply to server-only state
      lift $ interpretS dynamicsWorld fromAddr message

    -- Run the physics sim
    stepSimulation dynamicsWorld
    
    -- Generate a list of instructions updating 
    -- each object with the state from the physics sim
    rigidBodies <- lift $ use (ssRigidBodies . to Map.toList)
    players     <- lift $ use (ssPlayers     . to Map.toList)
    
    transientInstructions <- (++) 
      <$> forM rigidBodies (\(objID, rigidBody) -> do
            (pos, orient) <- getBodyState rigidBody
            let scale' = 1.0
            return $ UpdateObject objID (Object (Pose pos orient) scale'))
      <*> forM players (\(playerID, player) -> 
            return $ UpdatePlayer playerID player)

    -- Generate reliable instructions
    -- Add the cube to the queue, and delete the oldest cubes
    deleters <- Seq.drop maxCubes <$> lift (use ssCubeExpirationQueue)

    let reliableInstructions = DeleteObject <$> toList deleters

    forM_ reliableInstructions $ \instruction -> do
      interpret instruction
      broadcastToClients $ Reliable instruction

    -- Apply to our own copy of the world
    interpret `mapM_` transientInstructions

    -- Broadcast the simulation results to all clients
    broadcastToClients $ Unreliable transientInstructions

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
  ssRigidBodies . at objID ?= rigidBody

  -- Add the cube to the expiration queue
  ssCubeExpirationQueue %= (objID <|)

interpretS _dynamicsWorld _fromAddr (UpdatePlayer playerID player) = do
  ssPlayers . at playerID ?= player

  maybeHandRigidBodies <- use $ ssPlayerRigidBodies . at playerID
  forM_ maybeHandRigidBodies $ \handRigidBodies -> 
    forM_ (zip (player ^. plrHandPoses) handRigidBodies) $ \(handPose, handRigidBody) -> 
      setRigidBodyWorldTransform handRigidBody (handPose ^. posPosition) (handPose ^. posOrientation)

interpretS dynamicsWorld fromAddr (Connect playerID) = do
  -- Associate the playerID with the fromAddr we already know,
  -- so we can send an accurate disconnect message later
  ssPlayerIDs . at fromAddr ?= playerID

  handRigidBodies <- replicateM 2 $ do
    body <- addCube dynamicsWorld
                    mempty { scale    = handDimensions
                           }
    setRigidBodyKinematic body
    return body

  ssPlayerRigidBodies . at playerID ?= handRigidBodies

interpretS _dynamicsWorld fromAddr (Disconnect _) = 
  ssPlayerIDs . at fromAddr .= Nothing

interpretS dynamicsWorld _fromAddr (DeleteObject objID) = do

  rigidBody <- use $ ssRigidBodies . at objID
  maybe (return ()) (removeCube dynamicsWorld) rigidBody
  
  ssCubeExpirationQueue %= Seq.filter (/= objID)
  ssRigidBodies . at objID .= Nothing

interpretS _dynamicsWorld _fromAddr (UpdateObject _ _) = return ()



handleDisconnections :: (MonadIO m, MonadIO (t m), MonadTrans t, MonadState World (t m), MonadState ServerState m) 
                     => TChan SockAddr -> (AppPacket Op -> t m ()) -> DynamicsWorld -> t m ()
handleDisconnections disconnectionsChan broadcastToClients dynamicsWorld = do
  disconnections <- liftIO (atomically (exhaustChan disconnectionsChan))
  forM_ disconnections $ \fromAddr -> do
    -- For each SockAddr we've detected a disconnection from,
    -- find its associated player ID and broadcast a message to clients
    -- informing them that the player has left so they can clear any
    -- visible rendering of that player.

    maybePlayerID <- lift $ use $ ssPlayerIDs . at fromAddr
    case maybePlayerID of
      Nothing -> putStrLnIO $ "Couldn't find playerID for disconnecting address: " ++ show fromAddr
      Just playerID -> do
        let message = Disconnect playerID
        
        interpret message
        lift $ interpretS dynamicsWorld fromAddr message
        
        broadcastToClients (Reliable message)

        putStrLnIO $ "Goodbye: " ++ show fromAddr
