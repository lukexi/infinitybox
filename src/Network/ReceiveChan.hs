{-# LANGUAGE LambdaCase #-}
module Network.ReceiveChan where
import Data.Binary
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Trans
import Network.Socket.ByteString
import Network.Sox

makeReceiveChan :: (MonadIO m, Binary a) => Socket -> m (TChan a)
makeReceiveChan s = liftIO $ do
    messageChan <- newTChanIO
    void . forkIO . forever $ do
        message <- recv s 1024
        let decoded = decode' message
        atomically $ writeTChan messageChan decoded
    return messageChan

readChanAll :: MonadIO m => TChan a -> (a -> m b) -> m ()
readChanAll chan action = liftIO (atomically (tryReadTChan chan)) >>= \case
    Just msg -> action msg >> readChanAll chan action
    Nothing -> return ()