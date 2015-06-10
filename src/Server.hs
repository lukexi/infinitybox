{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
import Data.ByteString (ByteString)
import Network.Socket.ByteString

import Control.Monad
import Control.Monad.State
import Control.Monad.Free.Binary ()
import Control.Monad.Free.FromFreeT
import Control.Monad.Free

import Network.ReceiveChan
import Network.UDP.Pal hiding (newClientThread)
import Network.Socket hiding (sendTo)
import Control.Exception

import Types hiding (connect)

import Control.Concurrent
import Control.Concurrent.STM
import Linear

import Control.Lens
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Default 

import Physics.Bullet
import Pal.Physics

data ServerState = ServerState 
  { _ssRigidBodies :: Map ObjectID RigidBody
  , _ssPlayers     :: Map PlayerID Player
  }

newServerState :: ServerState
newServerState = ServerState mempty mempty

makeLenses ''ServerState

main :: IO ()
main = do
  putStrLn "Server engaged..."
  server <- makeServer serverName serverPort packetSize
  -- Receive messages on a background thread so we don't block
  receiveChan <- makeBinaryReceiveFromChan server

  clients       <- newMVar mempty
  broadcastChan <- newBroadcastTChanIO
  

  -- Initialize physics
  dynamicsWorld  <- createDynamicsWorld def { gravity = 0.0 }
  _              <- addBox dynamicsWorld (-10.0)

  
  
  let world = newWorld 0
  void . flip runStateT newServerState . flip runStateT world . forever $ do
    -- Receive updates from clients
    readChanAll receiveChan $ \(message, fromAddress) -> do
      let instructions = decode' message :: Instructions
      -- liftIO $ putStrLn $ "Got instructions " ++ show instructions
      -- Apply to our state
      interpret instructions
      
      lift $ interpretS dynamicsWorld instructions

      -- Update our clients list
      liftIO $ modifyMVar_ clients $ \currentClients -> 
        if Set.member fromAddress currentClients
        then return currentClients
        else do
          messageChan <- atomically $ dupTChan broadcastChan
          _ <- newClientThread fromAddress messageChan clients
          return $ Set.insert fromAddress currentClients

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
    players     <- lift $ use (ssPlayers     . to Map.toList)
    tickInstructions <- fromFreeT $ do 
      forM_ rigidBodies $  \(objID, rigidBody) -> do
        (pos, orient) <- getBodyState rigidBody
        let scale' = 1.0
        updateObject objID (Object pos orient scale')
      forM_ players $ \(playerID, player) -> 
        updatePlayer playerID player

    
    -- Apply to our own copy of the world
    interpret tickInstructions
    
    -- Encode and broadcast the simulation results
    let encoded = encode' tickInstructions

    -- Broadcast the message to all clients
    liftIO . atomically $ writeTChan broadcastChan encoded

    -- Run at 60 FPS
    liftIO $ threadDelay (1000000 `div` 60)


-- | Creates a new socket to the client's address, and creates a Chan that's
-- continuously listened to on a new thread and passed along to the new socket
newClientThread :: SockAddr -> TChan ByteString -> MVar (Set SockAddr) -> IO ThreadId
newClientThread clientAddr messageChan clients = forkIO $ do

  -- (hostName, serviceName) <- getSockAddrAddress clientAddr
  toClientSock <- boundSocket Nothing 0
  -- print =<< getSocketName toClientSock

  -- toClientSock <- connectedSocketToAddr clientAddr

  -- Have the thread close the socket and remove the client
  -- from the broadcast queue when an exception occurs
  (hostName, serviceName) <- getSockAddrAddress clientAddr
  let displayName = "->" ++ hostName ++ ":" ++ serviceName
      finisher e = do
        putStrLn $ displayName ++ " removing self due to " ++ show (e::SomeException)
        close toClientSock
        modifyMVar_ clients $ return . Set.delete clientAddr
        throwIO e
  putStrLn $ "Sending messages to " ++ displayName

  handle finisher . forever $ do    
    message <- atomically $ readTChan messageChan
    _bytesSent <- sendTo toClientSock message clientAddr
    return ()


-- | Interpret a client message 'updating' an object as creating that object
-- and add it to the physics world
interpretS :: (MonadIO m, MonadState ServerState m) => DynamicsWorld -> Free Op () -> m ()
interpretS dynamicsWorld = iterM interpret'
  where
    interpret' :: (MonadIO m, MonadState ServerState m) => Op (m t) -> m t
    interpret' (UpdateObject objID obj next) = do
      
      rigidBody <- addCube dynamicsWorld
                           def { position = obj ^. objPosition
                               , rotation = obj ^. objOrientation
                               , scale    = realToFrac (obj ^. objScale)
                               }

      -- Shoot the cube outwards
      let v = rotate ( obj ^. objOrientation ) ( V3 0 0 ( -3 ) )
      _ <- applyCentralForce rigidBody v
      ssRigidBodies . at objID ?= rigidBody
      next
    interpret' (UpdatePlayer playerID player next) = do
      ssPlayers . at playerID ?= player
      next
    interpret' (Connect _ next)     = next
  


