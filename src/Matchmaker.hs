module Matchmaker where

import Network.UDP.Pal
import Network.Socket
import Control.Concurrent
import Control.Monad
import Control.Exception
import Data.IORef
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

  ourIP <- findPrivateNetIP

  -- Begin trying to receive a server beacon message
  receiveSocket <- boundSocketAny broadcastPort (fromIntegral bufferSize)
  searchResultMVar <- newEmptyMVar
  stopSearchRef <- newIORef False
  searchThread <- forkIO $ do
    let loop = do
          stopSearch <- readIORef stopSearchRef
          unless stopSearch $ do
            putStrLn "Receiving..."
            receivedData <- receiveFromDecoded receiveSocket
            case receivedData of
              (receivedMagicNumber, SockAddrInet _port hostAddress) | receivedMagicNumber == magicNumber -> do
                hostAddressString <- inet_ntoa hostAddress
                if (hostAddressString /= ourIP)
                  then void $ tryPutMVar searchResultMVar hostAddressString  
                  else loop
              _ -> loop
    loop

  -- Search for 1 second
  putStrLn $ "Waiting for matchmaking..."
  threadDelay 1000000
  putStrLn $ "Matchmaking done."

  writeIORef stopSearchRef True
  close (bsSocket receiveSocket)
  putStrLn $ "Killed...."
  threadDelay 100000
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

