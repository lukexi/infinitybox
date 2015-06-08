{-# LANGUAGE NoMonomorphismRestriction #-}
module Network.Sox (
    module Network.Sox,
    Socket(..), SockAddr(..),
) where

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Control.Exception
import Data.Binary
import qualified Data.ByteString.Lazy as L
import Data.ByteString
import Control.Monad.Trans

port :: String
port = "3000"
ip :: String
--ip   = "192.168.0.14"
ip   = "127.0.0.1"

asClient :: (Socket -> IO c) -> IO c
asClient = withSocketsDo . bracket (clientSocket ip port) sClose


-- Connect a socket to a remote address
clientSocket :: HostName -> ServiceName -> IO Socket
clientSocket toAddress toPort = do
    (serverAddr:_) <- getAddrInfo Nothing (Just toAddress) (Just toPort)
    s <- socket (addrFamily serverAddr) Datagram defaultProtocol
    connect s (addrAddress serverAddr)
    return s


asServer :: (Socket -> IO c) -> IO c
asServer = withSocketsDo . bracket (serverSocket port) sClose

-- | Create a socket than can be listened to
serverSocket :: ServiceName -> IO Socket
serverSocket listenPort = do
    (serverAddr:_) <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) Nothing (Just listenPort)
    sock <- socket (addrFamily serverAddr) Datagram defaultProtocol
    bindSocket sock (addrAddress serverAddr)
    return sock

sendB :: (MonadIO m, Binary a) => Socket -> a -> m Int
sendB s = liftIO . send s . encode'

encode' :: Binary a => a -> ByteString
encode' = L.toStrict . encode

decode' :: Binary c => ByteString -> c
decode' = decode . L.fromStrict