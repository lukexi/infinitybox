{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
module Network.ReceiveChan where
import Data.Binary
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Trans
import Network.Socket hiding (send, sendTo, recv, recvFrom)

import Data.ByteString (ByteString)
import Data.Monoid

import Network.UDP.Pal

packetSize :: Int
packetSize = 64000
serverName :: HostName
serverName = "192.168.0.14"
serverPort :: PortNumber
serverPort = 3000

makeBinaryReceiveFromChan :: (MonadIO m) => Server -> m (TChan (ByteString, SockAddr))
makeBinaryReceiveFromChan server = liftIO $ do
  messageChan <- newTChanIO

  void . forkIO . forever $ 
    atomically . writeTChan messageChan =<< receiveFromRaw server    

  return messageChan

makeReceiveChan :: (MonadIO m, Binary a) => Client -> m (TChan a)
makeReceiveChan client = liftIO $ do
  messageChan <- newTChanIO
  
  void . forkIO . forever $ 
    atomically . writeTChan messageChan =<< fst <$> receiveFromDecoded client

  return messageChan

readChanAll :: (MonadIO m, Monoid b) => TChan a -> (a -> m b) -> m b
readChanAll chan action = go mempty 
  where go !accum = do 
          liftIO (atomically (tryReadTChan chan)) >>= \case
            Just msg -> do
              r <- action msg
              go (r <> accum)
            Nothing -> return accum