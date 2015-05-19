import qualified Data.ByteString as B
import Network.Socket.ByteString

import Control.Monad
import Control.Monad.Free.Binary ()

import Network.Sox

import Instructions

main :: IO ()
main = asServer $ \conn -> do
    putStrLn "Server engaged..."
    let go clients = do
            (message, fromAddress) <- recvFrom conn 1024
            let newClients = if fromAddress `elem` clients then clients else fromAddress:clients

            let decoded = decode' message :: Instructions
            putStrLn $ "<ECHOING: " ++ show decoded
            unless (B.null message) $ do

                forM_ newClients $ \clientAddr -> 
                    when (clientAddr /= fromAddress) $ do
                        putStrLn $ "Sending to " ++ show clientAddr
                        _bytesSent <- sendTo conn message clientAddr
                        return ()
                go newClients
    go []