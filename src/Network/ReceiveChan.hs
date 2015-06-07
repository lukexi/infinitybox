{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
module Network.ReceiveChan where
import Data.Binary
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Trans
import Control.Exception 
import Network.Socket.ByteString
import Network.Socket hiding (send, sendTo, recv, recvFrom)

import Network.Sox
import Data.ByteString (ByteString)
import Data.Monoid
import Data.IORef

packetSize :: Int
packetSize = 4096

makeBinaryReceiveFromChan :: (MonadIO m) => IORef Socket -> m (TChan (ByteString, SockAddr))
makeBinaryReceiveFromChan sockRef = liftIO $ do
  messageChan <- newTChanIO


  void . forkIO . forever $ do
    s <- readIORef sockRef
    try (recvFrom s packetSize) >>= \case
        Left err -> do
          putStrLn $ "Exception: " ++ show (err::SomeException)
          sClose s
          newSocket <- listenSocket serverPort
          writeIORef sockRef newSocket
          -- go newSocket
        Right message -> do
          
          atomically . writeTChan messageChan $ message
    

  return messageChan

makeReceiveChan :: (MonadIO m, Binary a) => Socket -> m (TChan a)
makeReceiveChan s = liftIO $ do
  messageChan <- newTChanIO
  void . forkIO . forever $ do
    message <- recv s packetSize
    let decoded = decode' message
    atomically $ writeTChan messageChan decoded
  return messageChan

readChanAll :: (MonadIO m, Monoid b) => TChan a -> (a -> m b) -> m b
readChanAll chan action = go mempty 
  where go !accum = do 
          liftIO (atomically (tryReadTChan chan)) >>= \case
            Just msg -> do
              r <- action msg
              go (r <> accum)
            Nothing -> return accum