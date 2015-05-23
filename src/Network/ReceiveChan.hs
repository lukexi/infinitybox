{-# LANGUAGE LambdaCase #-}
module Network.ReceiveChan where
import Data.Binary
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Trans
import Network.Socket.ByteString
import Network.Socket (SockAddr)
import Network.Sox
import Data.ByteString
import Data.Monoid

packetSize = 2048

makeBinaryReceiveFromChan :: (MonadIO m) => Socket -> m (TChan (ByteString, SockAddr))
makeBinaryReceiveFromChan s = liftIO $ do
    messageChan <- newTChanIO
    void . forkIO . forever $ do
        message <- recvFrom s packetSize
        atomically $ writeTChan messageChan message
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
    where go accum = do 
            liftIO (atomically (tryReadTChan chan)) >>= \case
                Just msg -> do
                    r <- action msg
                    go (r <> accum)
                Nothing -> return accum