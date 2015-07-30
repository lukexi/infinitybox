{-# LANGUAGE FlexibleContexts, LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
--module Client where
import Graphics.UI.GLFW.Pal

import Graphics.GL
import Control.Concurrent.STM

import Control.Monad
import Control.Monad.State
import System.Random
import Control.Lens hiding (view)
import Control.Monad.Random

import Sound.Pd1

import Network.UDP.Pal
import Game.Pal

import Types
import Resources
import Render
import Controls


enableVR :: Bool
-- enableVR = False
enableVR = True

enableHydra :: Bool
enableHydra = False
-- enableHydra = True

main :: IO ()
main = do
  -- Set up GLFW/Oculus/Hydra
  (window, events, maybeHMD, maybeRenderHMD, maybeSixenseBase) <- initWindow "Infinity Box" enableVR enableHydra  
  
  -- Set up sound
  patch <- makePatch "src/world"
  openALSources <- getPdSources
  metro1 <- makeReceiveChan (local patch "metro1")
  metro2 <- makeReceiveChan (local patch "metro2")

  -- Set up networking
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
  let world = newWorld playerID
  void . flip runRandT stdGen . flip runStateT world . whileWindow window $ do
    frameNumber <- wldFrameNumber <+= 1

    -- Update interpolation buffer
    wldLastCubes <~ use wldCubes

    -- Handle network events
    interpretNetworkPackets tcVerifiedPackets interpret

    -- Process controllers (Keyboard, Mouse, Gamepad, Hydra, Oculus headtracking)
    processControls window events maybeSixenseBase maybeHMD transceiver frameNumber

    -- Handle Pd events
    liftIO (atomically (exhaustChan metro1)) >>= mapM_ (\_ -> wldMetro1 .= 0)
    liftIO (atomically (exhaustChan metro2)) >>= mapM_ (\_ -> wldMetro2 .= 0)
    wldMetro1 += 0.01
    wldMetro2 += 0.01

    -- Send player position
    player <- use wldPlayer
    writeTransceiver transceiver $ Reliable $ UpdatePlayer playerID player

    -- Render to OpenAL
    -- Update AL listener
    Pose totalHeadPosit totalHeadOrient <- totalHeadPose
    alListenerPosition totalHeadPosit
    alListenerOrientation totalHeadOrient

    -- Update AL sources
    handWorldPoses <- use (wldPlayer . plrHandPoses)
    forM_ (zip openALSources handWorldPoses) $ \(sourceID, Pose posit _orient) -> do
      alSourcePosition sourceID posit

    -- Render to OpenGL
    case maybeRenderHMD of

      Nothing        -> renderFlat window    resources
      Just renderHMD -> renderVR   renderHMD resources


