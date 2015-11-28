{-# LANGUAGE CPP #-}
import Client
import Types
import Graphics.UI.GLFW.Pal

main :: IO ()
main = 
#if !defined(ENABLE_LOGGING)
  suppressConsole "infinitybox.log" >> 
#endif
    infinityClient UseLocalhost
