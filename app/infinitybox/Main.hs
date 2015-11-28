{-# LANGUAGE CPP #-}
import Client
import Types
#if !defined(ENABLE_LOGGING)
import Graphics.UI.GLFW.Pal
#endif

main :: IO ()
main = 
#if !defined(ENABLE_LOGGING)
  suppressConsole "infinitybox.log" >> 
#endif
    infinityClient UseLocalhost
