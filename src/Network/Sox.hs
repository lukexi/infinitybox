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
import Data.ByteString (ByteString)
import Control.Monad.Trans

serverPort :: String
serverPort = "3000"

clientPort :: String
clientPort = "3001"

ip :: String
--ip   = "192.168.0.14"
ip   = "127.0.0.1"

asClient :: (Socket -> IO c) -> IO c
asClient = withSocketsDo . bracket (socketToAddress ip serverPort) (\s -> putStrLn "Closing socket" >> sClose s)

asServer :: (Socket -> IO c) -> IO c
asServer = withSocketsDo . bracket (listenSocket serverPort) sClose

-- Connect a socket to a remote address
socketToAddress :: HostName -> ServiceName -> IO Socket
socketToAddress toAddress toPort = do
    (serverAddr:_) <- getAddrInfo Nothing (Just toAddress) (Just toPort)
    s <- socket (addrFamily serverAddr) Datagram defaultProtocol
    connect s (addrAddress serverAddr)
    return s

-- | Create a socket than can be listened to
listenSocket :: ServiceName -> IO Socket
listenSocket listenPort = do
    -- Configure for accepting connections
    let hints = Just $ defaultHints { addrFlags = [AI_PASSIVE] }
    (serverAddr:_) <- getAddrInfo hints Nothing (Just listenPort)
    sock <- socket (addrFamily serverAddr) Datagram defaultProtocol
    bindSocket sock (addrAddress serverAddr)
    return sock

-- | Send a 'Binary' value to a socket
sendB :: (MonadIO m, Binary a) => Socket -> a -> m Int
sendB s = liftIO . send s . encode'

-- | Encode a value to a strict bytestring
encode' :: Binary a => a -> ByteString
encode' = L.toStrict . encode

-- | Decode a value from a strict bytestring
decode' :: Binary c => ByteString -> c
decode' = decode . L.fromStrict