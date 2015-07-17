{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module Server where

import Control.Monad
import Control.Monad.State

import Network.UDP.Pal hiding (newClientThread)

import Control.Concurrent
import Control.Concurrent.STM
import Linear

import Control.Lens
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Default 

import Physics.Bullet
import Physics
import Types

data ServerState = ServerState 
  { _ssRigidBodies :: Map ObjectID RigidBody
  , _ssPlayers     :: Map PlayerID Player
  , _ssPlayerIDs   :: Map SockAddr PlayerID
  }

newServerState :: ServerState
newServerState = ServerState mempty mempty mempty

makeLenses ''ServerState

main :: IO ()
main = do
  putStrLn "Server engaged..."

  (getPacketsFromClients, broadcastToClients, disconnectionsChan) <- createServer serverName serverPort packetSize

  
  -- Initialize physics
  dynamicsWorld  <- createDynamicsWorld def { gravity = 0.0 }
  _              <- addBox dynamicsWorld (-10.0)
  
  
  let world = newWorld "Server"
  void . flip runStateT newServerState . flip runStateT world . forever $ do
    handleDisconnections disconnectionsChan broadcastToClients
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
            return $ UpdateObject objID (Object pos orient scale'))
      <*> forM players (\(playerID, player) -> 
            return $ UpdatePlayer playerID player)

    
    -- Apply to our own copy of the world
    forM_ transientInstructions interpret

    -- Broadcast the simulation results to all clients
    broadcastToClients $ Unreliable transientInstructions

    -- Run at 60 FPS
    liftIO $ threadDelay (1000000 `div` 60)



-- | Interpret a client message 'updating' an object as creating that object
-- and add it to the physics world
interpretS :: (MonadIO m, MonadState ServerState m) 
           => DynamicsWorld -> SockAddr -> Op -> m ()
interpretS dynamicsWorld _fromAddr (UpdateObject objID obj) = do
  
  rigidBody <- addCube dynamicsWorld
                       def { position = obj ^. objPosition
                           , rotation = obj ^. objOrientation
                           , scale    = realToFrac (obj ^. objScale)
                           }

  -- Shoot the cube outwards
  let v = rotate ( obj ^. objOrientation ) ( V3 0 0 ( -3 ) )
  _ <- applyCentralForce rigidBody v
  ssRigidBodies . at objID ?= rigidBody

interpretS _dynamicsWorld _fromAddr (UpdatePlayer playerID player) =
  ssPlayers . at playerID ?= player

interpretS _dynamicsWorld fromAddr (Connect playerID) = 
  -- Associate the playerID with the fromAddr we already know,
  -- so we can send an accurate disconnect message later
  ssPlayerIDs . at fromAddr ?= playerID
interpretS _dynamicsWorld _fromAddr (Disconnect _) = return ()
  


handleDisconnections :: (MonadIO (t m), MonadTrans t, MonadState World (t m), MonadState ServerState m) 
                     => TChan SockAddr -> (AppPacket Op -> t m ()) -> t m ()
handleDisconnections disconnectionsChan broadcastToClients = do
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
        lift $ ssPlayerIDs . at fromAddr .= Nothing
        interpret message
        
        broadcastToClients (Reliable message)

        putStrLnIO $ "Goodbye: " ++ show fromAddr
