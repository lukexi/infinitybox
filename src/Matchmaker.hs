module Matchmaker where

import Network.UDP.Pal
import Network.Socket
import Control.Concurrent
import Control.Monad
import Control.Exception
--------------- Broadcast socket utils
-- TODO move these to 

broadcastIP :: HostName
broadcastIP = "255.255.255.255"

broadcastSocket :: PortNumber -> PacketSize -> IO SocketWithDest
broadcastSocket port bufSize = do
  bcastSocket <- socketWithDest broadcastIP port bufSize
  setSocketOption (bsSocket (swdBoundSocket bcastSocket)) Broadcast 1
  return bcastSocket



-- | A bound socket that will listen on the given port on any/all IPs
boundSocketAny :: PortNumber -> PacketSize -> IO BoundSocket
boundSocketAny port bufSize = do
  anyHost <- inet_ntoa iNADDR_ANY
  boundSocket (Just anyHost) port bufSize


-- | A magic number to ensure we aren't decoding a spurious broadcast message
magicNumber :: Int
magicNumber = 12345

broadcastPort :: PortNumber
broadcastPort = 9999

bufferSize :: Integer
bufferSize = 4096

-- beginSearch :: IO ThreadId
beginSearch :: (HostName -> IO ()) -> IO () -> IO ThreadId
beginSearch onFoundHost onNoHost = forkIO $ do

  -- Begin trying to receive a server beacon message
  receiveSocket <- boundSocketAny broadcastPort (fromIntegral bufferSize)
  searchResultMVar <- newEmptyMVar
  searchThread <- forkIO $ do
    let loop = do
          putStrLn "Receiving..."
          receivedData <- receiveFromDecoded receiveSocket
          case receivedData of
            (receivedMagicNumber, SockAddrInet _port hostAddress) | receivedMagicNumber == magicNumber -> do
              hostAddressString <- inet_ntoa hostAddress
              tryPutMVar searchResultMVar hostAddressString
              return ()
            _ -> loop
    loop
  -- FIXME: need to kill the searchThread in the event it never sees a broadcast, 
  -- but it was somhow killing its parent or hanging on killthread?

  putStrLn $ "Waiting for matchmaking..."
  -- Search for 1 second
  threadDelay 1000000
  putStrLn $ "Matchmaking done."
  -- mask $ \_ -> killThread searchThread
  putStrLn $ "Killed...."
  -- Check the results
  searchResult <- tryReadMVar searchResultMVar
  putStrLn $ "Result of matchmaking: " ++ show searchResult
  case searchResult of
    Just foundServerIP -> onFoundHost foundServerIP
    Nothing            -> onNoHost

beginBroadcaster :: IO ThreadId
beginBroadcaster = do
  bcastSocket <- broadcastSocket broadcastPort 4096

  forkIO . forever $ do
    -- putStrLn "Tick!"
    _ <- sendBinary bcastSocket magicNumber
    threadDelay (1000000 `div` 2)

