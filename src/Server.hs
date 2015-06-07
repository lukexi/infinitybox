{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
import qualified Data.ByteString as B
import Network.Socket.ByteString

import Control.Monad
import Control.Monad.State
import Control.Monad.Free.Binary ()
import Control.Monad.Free.FromFreeT
import Control.Monad.Free

import Network.Sox
import Network.ReceiveChan

import Types

import Control.Concurrent
import Linear



import Control.Lens
import qualified Data.Map as Map
import Data.Map (Map)
import Data.List
import Data.Default 

import Physics.Bullet


data ServerState = ServerState 
  { _ssClients :: [SockAddr]
  , _ssRigidBodies :: Map ObjectID RigidBody
  }

newServerState :: ServerState
newServerState = ServerState mempty mempty

makeLenses ''ServerState

-- | Interpret a client message 'updating' an object as creating that object
-- and add it to the physics world
interpretS :: (MonadIO m, MonadState ServerState m) => DynamicsWorld -> Free Op () -> m ()
interpretS dynamicsWorld = iterM interpret'
  where
    interpret' :: (MonadIO m, MonadState ServerState m) => Op (m t) -> m t
    interpret' (Update objID obj n) = do

      let p = ( obj ^. objPosition    )
      let r = ( obj ^. objOrientation )
      let s = ( obj ^. objScale       )
      
      rigidBody <- addCube  dynamicsWorld
                            def { position = p
                                , rotation = r 
                                , scale    = V3 s s s
                                }

      let v = rotate ( obj ^. objOrientation ) ( V3 0 0 ( -3 ) )
      applyCentralForce rigidBody v
      ssRigidBodies . at objID ?= rigidBody
      n
    interpret' (Echo    _ n)     = n
    interpret' (Connect _ n)     = n


main :: IO ()
main = asServer $ \sock -> do
  putStrLn "Server engaged..."

  dynamicsWorld  <- createDynamicsWorld
  _              <- addGroundPlane dynamicsWorld
  
  -- Receive messages on a background thread so we don't block
  receiveChan <- makeBinaryReceiveFromChan sock

  void . flip runStateT newServerState . flip runStateT newWorld . forever $ do
    -- Receive updates from clients
    readChanAll receiveChan $ \(message, fromAddress) -> do
      let instructions = decode' message :: Instructions
      -- liftIO $ putStrLn $ "Got instructions " ++ show instructions
      -- Apply to our state
      interpret instructions
      lift $ do
        interpretS dynamicsWorld instructions

        -- Update our clients list
        ssClients %= nub . (fromAddress:)

        -- Rebroadcast to other clients
        -- sendInstructions sock message (Just fromAddress)
        -- Not rebroadcasting for now, since we don't
        -- want to send unsimulated objects to the clients
      return ()

    -- Run the physics sim
    stepSimulation dynamicsWorld
    
    -- Generate a list of instructions updating 
    -- each object with the state from the physics sim
    rigidBodies <- lift $ use (ssRigidBodies . to Map.toList)
    tickInstructions <- fromFreeT . forM_ rigidBodies $ 
      \(objID, rigidBody) -> do
        (pos, orient) <- getBodyState rigidBody
        let scale = 0.05
        update objID (Object pos orient scale )
    
    -- Apply to our own copy of the world
    interpret tickInstructions
    
    -- Encode and broadcast the simulation results
    let encoded = encode' tickInstructions
    lift $ sendInstructions sock encoded Nothing

    -- Run at 60 FPS
    liftIO $ threadDelay (1000000 `div` 60)

sendInstructions :: (MonadIO m, MonadState ServerState m) => Socket
                              -> B.ByteString
                              -> Maybe SockAddr
                              -> m ()
sendInstructions sock message fromAddress = do
  clients <- use ssClients
  forM_ clients $ \clientAddr -> 
    when (Just clientAddr /= fromAddress) $ do
      _bytesSent <- liftIO $ sendTo sock message clientAddr
      return ()
  
