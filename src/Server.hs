{-# LANGUAGE FlexibleContexts #-}
import qualified Data.ByteString as B
import Network.Socket.ByteString

import Control.Monad
import Control.Monad.State
import Control.Monad.Free.Binary ()
import Control.Monad.Free.FromFreeT

import Network.Sox
import Network.ReceiveChan

import Types

import Control.Concurrent
import Linear

import Control.Lens
import qualified Data.Map as Map
import Data.List

main :: IO ()
main = asServer $ \sock -> do
    putStrLn "Server engaged..."

    -- Receive messages on a background thread so we don't block
    receiveChan <- makeBinaryReceiveFromChan sock

    void . flip runStateT [] . flip runStateT newWorld . forever $ do
        -- Receive updates from clients
        readChanAll receiveChan $ \(message, fromAddress) -> do
            let instructions = decode' message :: Instructions
            -- Apply to our state
            interpret instructions

            -- Update our clients list
            lift $ id <>= [fromAddress]
            lift $ id %= nub

            -- Rebroadcast to other clients
            lift $ sendInstructions sock message fromAddress
            return ()
        
        -- Server runs simulation
        cubes <- use (wldCubes . to Map.toList)
        tickInstructions <- fromFreeT $ forM_ cubes $ \(objID, obj) -> do
            let vec = V3 0 0 (-0.5)
            let newObj = obj & objPosition +~ rotate (obj ^. objOrientation) vec
            update objID newObj
        interpret tickInstructions
        
        -- Encode and broadcast the simulation results
        let encoded = encode' tickInstructions
            fromNobody = SockAddrInet 3000 999
        lift $ sendInstructions sock encoded fromNobody

        liftIO $ threadDelay (1000000 `div` 30)

sendInstructions :: (MonadIO m, MonadState [SockAddr] m) => Socket
                                                         -> B.ByteString
                                                         -> SockAddr
                                                         -> m ()
sendInstructions sock message fromAddress = do
    clients <- get
    forM_ clients $ \clientAddr -> 
        when (clientAddr /= fromAddress) $ do
            _bytesSent <- liftIO $ sendTo sock message clientAddr
            return ()
    