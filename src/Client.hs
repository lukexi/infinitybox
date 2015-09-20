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
import Control.Lens hiding (view)
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
-- enableEKG = False

enableDevices :: [GamePalDevices]
enableDevices = [UseOculus, UseHydra]
-- enableDevices = [UseOculus]
--enableDevices = [UseHydra]
--enableDevices = []

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

main :: IO ()
main = do
  when enableEKG    . void $ EKG.forkServer "localhost" 8000
  
  -- Set up GLFW/Oculus/Hydra
  gamePal@GamePal{..} <- reacquire 0 $ initGamePal "Infinity Box" enableDevices  
  
  (voiceTicks, sourcesByVoice) <- initAudio
  -- let sourcesByVoice = mempty

  -- Set up networking
  serverName <- getServerNameFromFile
  transceiver@Transceiver{..} <- createTransceiverToAddress serverName serverPort packetSize

  -- Figure out if we're player 1 or 2
  localIP <- findLocalIP
  let initialPlayer = if localIP == serverName 
        then newPlayer1 
        else newPlayer2

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
    updateAudio voiceTicks

    -- Render to OpenGL
    wldCubeAges . traverse += 0.02
    --printIO =<< use wldCubeAges
    
    viewMat <- viewMatrixFromPose <$> use (wldPlayer . plrPose)
    renderWith gpWindow gpHMD viewMat 
      (glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT))
      (render theme)


