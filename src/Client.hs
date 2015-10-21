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
import System.Directory
import Data.Maybe
import Animation.Pal

import qualified System.Remote.Monitoring as EKG

import Network.UDP.Pal
import Game.Pal
import Data.Char
import Halive.Utils

import Types
import Themes
import Render
import Controls
import Audio


enableEKG :: Bool
enableEKG = False
-- enableEKG = True

enableDevices :: [GamePalDevices]
-- enableDevices = [UseOpenVR]
enableDevices = [UseOpenVR, UseHydra]
-- enableDevices = [UseOculus, UseHydra]
-- enableDevices = [UseOculus]
-- enableDevices = [UseHydra]
-- enableDevices = []


getServerNameFromFile :: IO String
getServerNameFromFile = do
  let serverIPFile = "serverIP.txt"
  exists <- doesFileExist serverIPFile
  if not exists 
    then return "127.0.0.1"
    else do 
      mLine <- fmap (filter (not . isSpace)) . listToMaybe . lines <$> readFile serverIPFile
      case mLine of
        Just line -> return line
        Nothing -> return "127.0.0.1"

infinityClient :: ServerIPType -> IO ()
infinityClient serverIPType = do
  when enableEKG    . void $ EKG.forkServer "localhost" 8000
  
  -- Set up GLFW/Oculus/Hydra
  gamePal@GamePal{..} <- reacquire 0 $ initGamePal "Infinity Box" GCPerFrame enableDevices
  
  (pitchesByVoice, amplitudesByVoice, sourcesByVoice) <- initAudio
  -- let sourcesByVoice = mempty

  -- Set up networking
  serverName <- case serverIPType of
    UseLocalhost -> return "127.0.0.1"
    UsePublicIP -> getServerNameFromFile

  transceiver@Transceiver{..} <- createTransceiverToAddress serverName serverPort packetSize

  -- Figure out if we're player 1 or 2
  localIP <- case serverIPType of
    UseLocalhost -> return "127.0.0.1"
    UsePublicIP  -> findLocalIP 
  let initialPlayer = if localIP == serverName 
        then newPlayer1 gpRoomScale
        else newPlayer2 gpRoomScale

  -- Connect to the server
  playerID <- randomName
  writeTransceiver transceiver $ Reliable (Connect playerID initialPlayer)

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
    wldLastCubes <~ use wldCubes

    -- Handle network events
    interpretNetworkPackets tcVerifiedPackets interpret

    -- Process controllers (Keyboard, Mouse, Gamepad, Hydra, Oculus headtracking)
    processControls gamePal transceiver frameNumber

    -- Send player position
    player <- use wldPlayer
    writeTransceiver transceiver $ Unreliable [UpdatePlayer playerID player]

    -- Render to OpenAL
    updateAudio pitchesByVoice amplitudesByVoice

    delta <- realToFrac <$> liftIO gpGetDelta

    -- Render to OpenGL
    wldCubeAges . traverse += delta

    phase <- use wldPhase
    
    when (phase /= PhaseVoid) $ wldTime += delta
    t <- use wldTime

    -- wldPhase   .= PhaseMain
    -- wldStarted .= 1

    --real one 
    when (phase == PhaseLogo && t > 11.0 ) $ do
      wldPhase   .= PhaseMain
      wldTime    .= 0
      wldStarted .= 1


    viewMat <- viewMatrixFromPose <$> use (wldPlayer . plrPose)
    renderWith gamePal viewMat 
      (glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT))
      (render theme)


