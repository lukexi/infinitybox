{-# LANGUAGE FlexibleContexts, LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
--module Client where
import Graphics.UI.GLFW.Pal

import Graphics.GL
import Graphics.Oculus
import Linear
import Control.Concurrent.STM
import System.Hardware.Hydra

import Control.Monad
import Control.Monad.State
import System.Random
import Control.Lens hiding (view)
import Control.Monad.Random

import Sound.Pd1

import Network.UDP.Pal

import Types
import Resources
import Render
import Controls


enableVR :: Bool
--enableVR = False
enableVR = True

enableHydra :: Bool
--enableHydra = False
enableHydra = True

main :: IO ()
main = do
  -- Set up Hydra
  sixenseBase <- if enableHydra then Just <$> initSixense else return Nothing
  
  patch <- makePatch "src/world"
  openALSources <- getPdSources
  metro1 <- makeReceiveChan (local patch "metro1")
  metro2 <- makeReceiveChan (local patch "metro2")

  -- Create a UDP receive thread
  transceiver@Transceiver{..} <- createTransceiverToAddress serverName serverPort packetSize

  -- Initialize GLFW
  (resX, resY, maybeHMD) <- if enableVR 
    then do
      hmd <- createHMD
      (resX, resY) <- getHMDResolution hmd
      return (resX, resY, Just hmd)
    else return (1024, 768, Nothing)
  
  (window, events) <- createWindow "UDPCubes" resX resY
  -- Compensate for retina framebuffers on Mac
  (frameW, frameH) <- getFramebufferSize window
  when (frameW > resX && frameH > resY) $
    setWindowSize window (resX `div` 2) (resY `div` 2)

  -- Set up Oculus
  maybeRenderHMD <- forM maybeHMD $ \hmd -> do
    renderHMD <- configureHMDRendering hmd "UDPCubes"
    dismissHSWDisplay hmd
    recenterPose hmd
    return renderHMD

  -- Get a stdgen for Entity ID generation
  stdGen   <- getStdGen
  
  -- Connect to the server
  -- Initial connection to the server
  playerID <- randomName
  writeTransceiver transceiver $ Reliable (Connect playerID)


  resources@Resources{..} <- loadResources


  -- Set up GL state
  glEnable GL_DEPTH_TEST
  glClearColor 0 0 0.1 1
  --glPolygonMode GL_FRONT_AND_BACK GL_LINE 
  
  -- Begin game loop
  let world = newWorld playerID
  void . flip runRandT stdGen . flip runStateT world . whileWindow window $ do
    frameNumber <- wldFrameNumber <+= 1

    -- Update interpolation buffer
    wldLastCubes <~ use wldCubes

    -- Handle network events
    interpretNetworkPackets tcVerifiedPackets interpret

    -- Process controllers (Keyboard, Mouse, Gamepad, Hydra, Oculus headtracking)
    processControls window events sixenseBase maybeHMD transceiver frameNumber

    -- Handle Pd events
    liftIO (atomically (exhaustChan metro1)) >>= mapM_ (\_ -> wldMetro1 .= 0)
    liftIO (atomically (exhaustChan metro2)) >>= mapM_ (\_ -> wldMetro2 .= 0)
    wldMetro1 += 0.01
    wldMetro2 += 0.01

    -- Send player position
    player <- use wldPlayer
    writeTransceiver transceiver $ Reliable $ UpdatePlayer playerID player

    -- Render to OpenAL
    Pose totalHeadPosit totalHeadOrient <- totalHeadPose
    alListenerPosition totalHeadPosit
    alListenerOrientation totalHeadOrient

    handWorldPoses <- use (wldPlayer . plrHandPoses)
    forM_ (zip openALSources handWorldPoses) $ \(sourceID, Pose posit _orient) -> do
      alSourcePosition sourceID posit

    -- Render to OpenGL
    case maybeRenderHMD of

      Nothing        -> renderFlat window    resources
      Just renderHMD -> renderVR   renderHMD resources

totalHeadPose :: (MonadState World m) => m Pose
totalHeadPose = do
  Pose playerPosit playerOrient <- use (wldPlayer . plrPose)
  Pose headPosit headOrient     <- use (wldPlayer . plrHeadPose)
  return $ Pose 
    (playerPosit + headPosit) 
    (playerOrient * conjugate headOrient)