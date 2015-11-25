{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts, LambdaCase #-}

module Controls where

import Graphics.UI.GLFW.Pal

import Linear.Extra


import Control.Monad
import Control.Monad.State.Strict
import Control.Lens.Extra hiding (view)
import Control.Monad.Random
import Graphics.GL

import Network.UDP.Pal
import Types
import Interpret

import Graphics.VR.Pal
import Data.Maybe
import Sound.Pd


import qualified Data.Map as Map


import Control.Concurrent


processControls :: (MonadIO m, MonadState World m, MonadRandom m) 
                => PureData
                -> VRPal
                -> MVar (Transceiver Op)
                -> Integer
                -> m ()
processControls pd vrPal@VRPal{..} transceiverMVar frameNumber = do
  -- Get latest Hydra data

  (hands, handsType) <- getHands vrPal

  -- Update hand positions
  handWorldPoses <- flip handsToWorldPoses hands . transformationFromPose <$> use (wldPlayer . plrPose)
  wldPlayer . plrHandPoses .= map poseFromMatrix handWorldPoses
  
  -- wldPlayer . plrHandPoses .= [newPose,newPose]

  -- Update head position
  wldPlayer . plrHeadPose <~ poseFromMatrix <$> getPoseForHMDType gpHMD

  -- Handle Hydra movement events, or mouse if no Hydra present
  if null hands 
    then do
      -- Disabled mouselook because it's dumb
      -- isFocused <- getWindowFocused window
      -- when isFocused $ applyMouseLook window (wldPlayer . plrPose)
      return ()
    else do
      -- Disabled hydra joysticks for no motion sickness
      when (handsType == HandsHydra) $
        applyHandJoystickMovement hands (wldPlayer . plrPose)
      return ()
  
  -- Handle keyboard movement events
  applyWASD gpWindow (wldPlayer . plrPose)
  
  -- Handle UI events
  processEvents gpEvents $ \e -> do
    closeOnEscape gpWindow e

    applyGamepadJoystickMovement e (wldPlayer . plrPose)

    -- Get player pose
    playerPose <- use (wldPlayer . plrPose)

    onGamepadAxes e $ \GamepadAllAxes{..} -> 
      -- Use the right trigger to fire a cube
      when (gaxTriggers < (-0.5) && frameNumber `mod` 30 == 0) $ 
        addCube pd vrPal transceiverMVar (shiftBy (V3 0 0.1 0) playerPose)
    
    -- Handle key events
    -- Spawn a cube offset by 0.1 y
    onKeyDown e Key'Space (addCube pd vrPal transceiverMVar (shiftBy (V3 0 0.1 0) playerPose))
    onKeyDown e Key'F (setCursorInputMode gpWindow CursorInputMode'Disabled)
    onKeyDown e Key'G (setCursorInputMode gpWindow CursorInputMode'Normal)
    onKeyDown e Key'O (recenterWhenOculus vrPal)
    onKeyDown e Key'Z (addCube pd vrPal transceiverMVar newPose)
    onKeyDown e Key'N (startLogo pd)
    onKeyDown e Key'M startMain
    onKeyDown e Key'C clonePlayer
    onKeyDown e Key'0 (restart transceiverMVar)

  xDown <- (== KeyState'Pressed) <$> getKey gpWindow Key'X

  -- Til I finish per-hand vacuuming, vacuum when either bumper is down
  let shouldVacuum = or (map (^. hndGrip) hands) || xDown
  wldPlayer . plrVacuum .= shouldVacuum

  -- Fire cubes from each hand when their triggers are held down
  forM_ (zip hands handWorldPoses) $ \(hand, handMatrix) -> do

    -- Bind Hydra 'Start' buttons to HMD Recenter
    when (hand ^. hndButtonS) $ recenterWhenOculus vrPal

    processHandCubeFiring pd vrPal hand (poseFromMatrix handMatrix) frameNumber transceiverMVar
  wldLastHands .= Map.fromList (zip (map (^. hndID) hands) hands)



processHandCubeFiring :: (Integral a, MonadIO m, MonadState World m, MonadRandom m) 
                      => PureData -> VRPal -> Hand -> Pose GLfloat -> a -> MVar (Transceiver Op) -> m ()    
processHandCubeFiring pd vrPal hand handPose _frameNumber transceiverMVar  = do

  -- Determine if the trigger is freshly pressed
  lastHand <- fromMaybe emptyHand <$> use (wldLastHands . at (hand ^. hndID))

  let triggerIsDown = hand ^. hndTrigger > 0.5
      triggerWasDown = lastHand ^. hndTrigger > 0.5
      isNewTrigger  = triggerIsDown && not triggerWasDown

  -- Determine if the start button is freshly pressed
  -- Sample the current player position and write it as a dummy remote player
  -- to allow inspecting what a multiplayer person looks like
  -- when (not (lastHand ^. hndButtonS) && hand ^. hndButtonS) $ 
  --   clonePlayer

  phase <- use wldPhase

  case phase of
    PhaseVoid -> when triggerIsDown $ startLogo pd
    PhaseLogo -> return ()
    _ -> do
      -- Move the cube upwards a bit so it spawns at the tip of the hand
      let cubePose = shiftBy (V3 0 0 (-0.5)) handPose
      -- Spawn every 0.1 secs, and on fresh trigger squeezes
          shouldSpawn = isNewTrigger
                        -- || triggerIsDown && frameNumber `mod` 30 == 0
      when shouldSpawn $
        addCube pd vrPal transceiverMVar cubePose


addCube :: (MonadIO m, MonadState World m, MonadRandom m) 
        => PureData -> VRPal -> MVar (Transceiver Op) -> Pose GLfloat -> m ()
addCube pd vrPal transceiverMVar pose = do
  -- Spawn a cube at the player's position and orientation
  objID <- getRandom
  -- Ugly: need to have the cube instantly small so we don't render it full size temporarily,
  -- but the server needs to know its full size since it uses that internally. Thus
  -- we need two messages to hide the latency...
  let serverInstruction = CreateObject objID (Object pose cubeScale)
      clientInstruction = CreateObject objID (Object pose initialCubeScale)
  interpret pd vrPal clientInstruction

  whenMVar transceiverMVar $ \transceiver -> 
    writeTransceiver transceiver $ Reliable serverInstruction
  return ()


startLogo :: (MonadState World m, MonadIO m) => PureData -> m ()
startLogo pd = do
  wldPhase .= PhaseLogo
  wldTime  .= 0
  sendGlobal pd "startLogo" Bang

startMain :: (MonadState World m) => m ()
startMain = do
  wldPhase .= PhaseMain
  wldTime  .= 0

restart :: (MonadState World m, MonadIO m) => MVar (Transceiver Op) -> m ()
restart transceiverMVar = do
  whenMVar transceiverMVar $ \transceiver ->
    writeTransceiver transceiver $ Reliable Restart
  wldStarted .= 0
  wldPhase .= PhaseVoid
  wldTime  .= 0





whenMVar :: MonadIO m => MVar a -> (a -> m ()) -> m ()
whenMVar mvar action = do
  maybeFull <- liftIO (tryReadMVar mvar)
  case maybeFull of
    Just full -> action full
    Nothing -> return ()


clonePlayer :: (MonadIO m, MonadState World m) => m ()
clonePlayer = do
    player <- use wldPlayer

    let infiniteSnapshots = True
    dummyID <- if infiniteSnapshots then liftIO randomName else return "dummy"

    wldPlayers . at dummyID ?= player
