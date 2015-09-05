{-# LANGUAGE FlexibleContexts, LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Client where
import Graphics.UI.GLFW.Pal

import Graphics.GL
import Graphics.GL.Pal2

import Control.Monad
import Control.Monad.State.Strict
import System.Random
import Control.Lens hiding (view)
import Control.Monad.Random
import Control.Concurrent
import System.Directory
import Data.Maybe

import qualified System.Remote.Monitoring as EKG

import Network.UDP.Pal
import Game.Pal
import Data.Char

import Types
import Resources
import Render
import Controls
import Server
import Audio


enableEKG :: Bool
enableEKG = False
-- enableEKG = False

enableServer :: Bool
-- enableServer = True
enableServer = False

enableVR :: Bool
enableVR = True
-- enableVR = False

enableHydra :: Bool
enableHydra = True
-- enableHydra = False

getServerNameFromFile :: IO String
getServerNameFromFile = do
  let serverIPFile = "serverIP.cfg"
  exists <- doesFileExist serverIPFile
  if not exists 
    then return "127.0.0.1"
    else do 
      line <- fmap (filter (not . isSpace)) . listToMaybe . lines <$> readFile serverIPFile
      case line of
        Just line -> return line
        Nothing -> return "127.0.0.1"

main :: IO ()
main = do
  when enableEKG    . void $ EKG.forkServer "localhost" 8000
  when enableServer . void $ forkOS physicsServer
  -- Set up GLFW/Oculus/Hydra
  (window, events, maybeHMD, maybeRenderHMD, maybeSixenseBase) <- initWindow "Infinity Box" enableVR enableHydra  
  
  (voiceTicks, sourcesByVoice) <- initAudio

  -- Set up networking
  serverName <- getServerNameFromFile
  transceiver@Transceiver{..} <- createTransceiverToAddress serverName serverPort packetSize

  -- Connect to the server
  playerID <- randomName
  writeTransceiver transceiver $ Reliable (Connect playerID)

  -- Set up OpenGL resources
  resources@Resources{..} <- loadResources


  -- Set up GL state
  glEnable GL_DEPTH_TEST
  glClearColor 0 0 0.1 1
  --glPolygonMode GL_FRONT_AND_BACK GL_LINE 
  
  -- Begin game loop
  -- Get a stdgen for Entity ID generation
  stdGen   <- getStdGen
  let world = newWorld playerID sourcesByVoice
  void . flip runRandT stdGen . flip runStateT world . whileWindow window $ do
    frameNumber <- wldFrameNumber <+= 1

    -- Update interpolation buffer
    wldLastCubes <~ use wldCubes

    -- Handle network events
    interpretNetworkPackets tcVerifiedPackets interpret

    -- Process controllers (Keyboard, Mouse, Gamepad, Hydra, Oculus headtracking)
    processControls window events maybeSixenseBase maybeHMD transceiver frameNumber

    -- Send player position
    player <- use wldPlayer
    writeTransceiver transceiver $ Unreliable [UpdatePlayer playerID player]

    -- Render to OpenAL
    updateAudio voiceTicks

    -- Render to OpenGL
    
    viewMat <- viewMatrixFromPose <$> use (wldPlayer . plrPose)
    renderWith window maybeRenderHMD viewMat 
      (glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT))
      (render resources)


