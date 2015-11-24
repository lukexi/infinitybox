import Client
import Types
import Graphics.UI.GLFW.Pal

main :: IO ()
main = suppressConsole "infinitybox-multiplayer.log" >> infinityClient UsePublicIP
