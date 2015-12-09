{-# LANGUAGE FlexibleContexts, LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Client where
import Graphics.UI.GLFW.Pal

import Graphics.GL
import Graphics.GL.Pal
import Sound.Pd

import Control.Monad
import Control.Monad.State.Strict
import System.Random
import Control.Lens.Extra
import qualified Data.Map.Strict as Map
import Data.Foldable
import Data.Maybe
import Control.Monad.Random
import Animation.Pal
import qualified System.Remote.Monitoring as EKG

import Network.UDP.Pal
import Graphics.VR.Pal
import Halive.Utils
import Control.Concurrent

import Types
import Shapes
import Render
import Controls
import Audio
import Server
import Matchmaker
import Interpret


enableEKG :: Bool
enableEKG = False
-- enableEKG = True

enableDevices :: [VRPalDevices]
enableDevices = [UseOpenVR]
-- enableDevices = [UseOpenVR, UseHydra]
-- enableDevices = [UseOculus, UseHydra]
-- enableDevices = [UseOculus]
-- enableDevices = [UseHydra]
-- enableDevices = []



infinityClient :: ServerIPType -> IO ()
infinityClient serverIPType = withPd $ \pd -> do
  when enableEKG    . void $ EKG.forkServer "localhost" 8000
  
  -- Set up GLFW/Oculus/Hydra
  vrPal@VRPal{..} <- reacquire 0 $ initVRPal "Infinity Box" enableDevices

  sourcesByVoice <- initAudio pd
  
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
  (shapes, uboBuffer) <- loadShapes

  -- Set up GL state
  glEnable GL_DEPTH_TEST
  glClearColor 0 0 0.1 1
  --glPolygonMode GL_FRONT_AND_BACK GL_LINE 
  
  -- Begin game loop
  -- Get a stdgen for Entity ID generation

  stdGen   <- getStdGen
  now      <- getNow
  let world = newWorld playerID initialPlayer sourcesByVoice now

  void . flip runRandT stdGen . flip runStateT world . whileVR vrPal $ \headM44 hands -> do
  
    frameNumber <- wldFrameNumber <+= 1

    -- Update interpolation buffer
    wldLastCubes   <~ use wldCubes
    wldLastPlayers <~ use wldPlayers
    
    -- Handle network events
    whenMVar transceiverMVar $ \transceiver -> 
      interpretNetworkPackets (tcVerifiedPackets transceiver) (interpret pd vrPal)

    -- Process controllers (Keyboard, Mouse, Gamepad, Hydra, Oculus headtracking)
    processControls pd vrPal transceiverMVar frameNumber

    -- Send player position
    player <- use wldPlayer

    whenMVar transceiverMVar $ \transceiver -> 
      writeTransceiver transceiver $ Unreliable [UpdatePlayer playerID player]

    -- Render to OpenAL
    updateAudio pd

    delta <- realToFrac <$> liftIO gpGetDelta

    -- Render to OpenGL

    phase <- use wldPhase
    
    when (phase /= PhaseVoid) $ wldTime += delta
    t <- use wldTime
    
    -- Begin the main phase of the experience after 11 seconds
    when (phase == PhaseLogo && t > 11.0) $ do
      wldPhase   .= PhaseMain
      wldTime    .= 0
      wldStarted .= 1

    -- DEBUG: Jump straight to main
    -- wldPhase   .= PhaseMain
    -- wldStarted .= 1

    immutably $ do
      players <- toList <$> (Map.unionWith interpolatePlayers
        <$> view wldLastPlayers
        <*> view wldPlayers)
      -- Interpolate between the last and newest cube states
      cubes <- Map.unionWith interpolateObjects 
        <$> view wldLastCubes 
        <*> view wldCubes

      lights <- updateUBO uboBuffer


      player <- view (wldPlayer . plrPose)
      renderWith vrPal player headM44 
        (glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT))
        (render shapes players cubes lights)


updateUBO uboBuffer = do
  lights12 <- getLocalHandPositions
  lights34 <- getFirstRemoteHandPositions
  let [V3 uLight1X uLight1Y uLight1Z, V3 uLight2X uLight2Y uLight2Z] = case lights12 of
        lights@[light1, light2] -> lights
        _ -> [0,0]
  -- let [V3 uLight3X uLight3Y uLight3Z, V3 uLight4X uLight4Y uLight4Z] = case lights34
  --       lights@[light3, light4] -> lights
  --       _ -> [0,0]

  now <- getNow
  fillednessAnim <- view wldFilledness
  let uFilledness = evanResult (evalAnim now fillednessAnim)

  uTime <- view wldTime
  uStarted <- view wldStarted
  uDayNight <- dayNightCycleAt <$> view wldTime
  let uDayLength = dayLength

  kickVoiceID <- view wldKickVoiceID
  uTick <- fromMaybe 0 <$> view (wldVoicePitch . at kickVoiceID)
  let uboData = [ uStarted
                , uTime
                , uDayNight
                , uDayLength
                
                , uLight1X, uLight1Y, uLight1Z
                , uFilledness

                , uLight2X, uLight2Y, uLight2Z
                , uTick
                ]
  bufferUniformSubData uboBuffer uboData

  {- We must ensure we group values by 4 for std140, 
     so keep an up to date reference of our UBO layout here to match with

  layout (std140) uniform uboData {
    // Manually grouping things into 4 'til I integrate a more principled approach
    uniform float uStarted;
    uniform float uTime;
    uniform float uDayNight;
    uniform float uDayLength;
    
    uniform vec3 light1;
    uniform float uFilledness;

    uniform vec3 light2;
    uniform float uTick;
  };

  -}


  -- Returned solely for debug drawing
  let lights = lights12 ++ lights34
  return lights