{-# LANGUAGE FlexibleContexts, LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Client where
import Graphics.UI.GLFW.Pal

import Graphics.GL
import Graphics.GL.Pal

import Control.Monad
import Control.Monad.State.Strict
import System.Random
import Control.Lens.Extra hiding (view)
import Control.Monad.Random
import Animation.Pal

import qualified System.Remote.Monitoring as EKG

import Network.UDP.Pal
import Graphics.VR.Pal
import Halive.Utils
import Control.Concurrent

import Types
import Themes
import Render
import Controls
import Audio
import Server
import Matchmaker

enableEKG :: Bool
enableEKG = False
-- enableEKG = True

enableDevices :: [VRPalDevices]
-- enableDevices = [UseOpenVR]
enableDevices = [UseOpenVR, UseHydra]
-- enableDevices = [UseOculus, UseHydra]
-- enableDevices = [UseOculus]
-- enableDevices = [UseHydra]
-- enableDevices = []

infinityClient :: ServerIPType -> IO ()
infinityClient serverIPType = do
  when enableEKG    . void $ EKG.forkServer "localhost" 8000
  
  -- Set up GLFW/Oculus/Hydra
  vrPal@VRPal{..} <- reacquire 0 $ initVRPal "Infinity Box" NoGCPerFrame enableDevices
  
  (pitchesByVoice, amplitudesByVoice, sourcesByVoice) <- initAudio
  
  -- Set up networking
  transceiverMVar <- newEmptyMVar

  playerID <- randomName

  let initialPlayer = newPlayer gpRoomScale
  let startTransceiverToServer serverName = do
        transceiver <- createTransceiverToAddress serverName serverPort packetSize
        putMVar transceiverMVar transceiver

        -- Connect to the server
        
        writeTransceiver transceiver $ Reliable (Connect playerID initialPlayer)
  case serverIPType of
    UseLocalhost -> do
      _ <- forkOS (physicsServer UseLocalhost)
      startTransceiverToServer "127.0.0.1"
    UsePublicIP -> do
      let onFoundServer = startTransceiverToServer
          onNoServer = do
            -- Same as finding the server, but connect to ourselves
            ourIP <- findPrivateNetIP
            putStrLn ("No existing server found, starting our own on " ++ show ourIP)
            _ <- forkOS (physicsServer UsePublicIP)
            startTransceiverToServer ourIP

      _ <- beginSearch onFoundServer onNoServer
      -- onNoServer
      return ()

  -- Set up OpenGL resources
  themes@Themes{..} <- loadThemes

  -- Set up GL state
  glEnable GL_DEPTH_TEST
  glClearColor 0 0 0.1 1
  --glPolygonMode GL_FRONT_AND_BACK GL_LINE 
  
  -- Begin game loop
  -- Get a stdgen for Entity ID generation

  stdGen   <- getStdGen
  now      <- getNow
  let world = newWorld playerID initialPlayer sourcesByVoice now
      theme = themes ^. rainbow

  void . flip runRandT stdGen . flip runStateT world . whileWindow gpWindow $ do
  
    frameNumber <- wldFrameNumber <+= 1

    -- Update interpolation buffer
    wldLastCubes   <~ use wldCubes
    wldLastPlayers <~ use wldPlayers
    
    -- Handle network events
    whenMVar transceiverMVar $ \transceiver -> 
      interpretNetworkPackets (tcVerifiedPackets transceiver) interpret

    -- Process controllers (Keyboard, Mouse, Gamepad, Hydra, Oculus headtracking)
    processControls vrPal transceiverMVar frameNumber

    -- Send player position
    player <- use wldPlayer

    whenMVar transceiverMVar $ \transceiver -> 
      writeTransceiver transceiver $ Unreliable [UpdatePlayer playerID player]

    -- Render to OpenAL
    updateAudio pitchesByVoice amplitudesByVoice

    delta <- realToFrac <$> liftIO gpGetDelta

    -- Render to OpenGL
    wldCubeAges . traverse += delta

    phase <- use wldPhase
    
    when (phase /= PhaseVoid) $ wldTime += delta
    t <- use wldTime

    -- DEBUG: Jump straight to main
    -- wldPhase   .= PhaseMain
    -- wldStarted .= 1
    
    -- Begin the main phase of the experience after 11 seconds
    when (phase == PhaseLogo && t > 11.0) $ do
      wldPhase   .= PhaseMain
      wldTime    .= 0
      wldStarted .= 1


    viewMat <- viewMatrixFromPose <$> use (wldPlayer . plrPose)
    renderWith vrPal viewMat 
      (glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT))
      (render theme)


