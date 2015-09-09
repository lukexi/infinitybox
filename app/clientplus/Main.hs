import qualified Client as Client
import qualified Server as Server
import Control.Concurrent

main :: IO ()
main = do
  _ <- forkOS Server.physicsServer
  Client.main
