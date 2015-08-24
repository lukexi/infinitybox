{-# LANGUAGE FlexibleContexts, LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Client where
import Graphics.UI.GLFW.Pal

import Graphics.GL
import Graphics.GL.Pal2
import Control.Concurrent.STM
import Linear

import Control.Monad
import Control.Monad.State.Strict
import System.Random
import Control.Lens hiding (view)
import Control.Monad.Random
import Control.Concurrent

import Sound.Pd1

import Network.UDP.Pal
import Game.Pal

import Types
import Resources
import Render
import Controls
import Server
import qualified Data.Map as Map

--import qualified System.Remote.Monitoring as EKG

enableEKG :: Bool
enableEKG = True

enableServer :: Bool
enableServer = True

enableVR :: Bool
-- enableVR = False
enableVR = True

enableHydra :: Bool
enableHydra = True
-- enableHydra = True

main :: IO ()
main = do
  --when enableEKG    . void $ EKG.forkServer "localhost" 8000
  when enableServer . void $ forkOS physicsServer
  -- Set up GLFW/Oculus/Hydra
  (window, events, maybeHMD, maybeRenderHMD, maybeSixenseBase) <- initWindow "Infinity Box" enableVR enableHydra  
  
  -- Set up sound
  _multiDAC <- makePatch "src/multidac"
  openALSources <- getPdSources
  patches <- foldM (\accum sourceID -> do
    let channelNum = length accum + 1
    voice <- makePatch "src/voice"
    send voice "set-channel" (Atom (String $ "dac" ++ show channelNum))
    --send voice "speed" (Atom (Float (realToFrac (channelNum * 100))))
    alSourcePosition sourceID (V3 0 0 (-10000) :: V3 GLfloat)

    output <- makeReceiveChan (local voice "output")
    return (accum ++ [(sourceID, voice, output)])
    ) [] openALSources 
  --patch <- makePatch "src/world"
  
  --metro1 <- makeReceiveChan (local patch "metro1")
  --metro2 <- makeReceiveChan (local patch "metro2")

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

    -- Send player position
    player <- use wldPlayer
    writeTransceiver transceiver $ Reliable $ UpdatePlayer playerID player

    -- Render to OpenAL
    -- Update AL listener
    Pose totalHeadPosit totalHeadOrient <- totalHeadPose
    alListenerPosition totalHeadPosit
    alListenerOrientation totalHeadOrient

    -- Handle Pd events
    -- Update AL sources
    --handWorldPoses <- use (wldPlayer . plrHandPoses)
    --forM_ (zip openALSources handWorldPoses) $ \(sourceID, Pose posit _orient) -> do
    --  alSourcePosition sourceID posit
    cubes <- use wldCubes
    forM_ (zip (Map.toList cubes) patches) $ \((cubeID, cubeObj), (sourceID, _patch, output)) -> do
      alSourcePosition sourceID (cubeObj ^. objPose . posPosition)
      exhaustChanIO output >>= mapM_ (\val -> 
        case val of
          Atom (Float f) -> wldPatchOutput . at cubeID ?= realToFrac f
          _ -> return ()
        )

    -- Render to OpenGL
    
    viewMat <- viewMatrixFromPose <$> use (wldPlayer . plrPose)
    renderWith window maybeRenderHMD viewMat 
      (glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT))
      (render resources)

exhaustChanIO :: MonadIO m => TChan a -> m [a]
exhaustChanIO = liftIO . atomically . exhaustChan
