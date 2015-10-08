import qualified Client as Client
import qualified Server as Server
import Control.Concurrent
import Types

main :: IO ()
main = do
  _ <- forkOS (Server.physicsServer UseLocalhost)
  Client.infinityClient UseLocalhost
