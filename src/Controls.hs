{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts, LambdaCase #-}

module Controls where

import Graphics.UI.GLFW.Pal

import Graphics.Oculus
import Linear.Extra


import Control.Monad
import Control.Monad.State.Strict
import Control.Lens.Extra hiding (view)
import Control.Monad.Random
import Graphics.GL

import Network.UDP.Pal
import Types

import Game.Pal
import Data.Maybe
import Sound.Pd1




processControls :: (MonadIO m, MonadState World m, MonadRandom m) 
                => GamePal
                -> Transceiver Op
                -> Integer
                -> m ()
processControls gamePal@GamePal{..} transceiver frameNumber = do
  -- Get latest Hydra data
  hands <- getHands gamePal

  -- Update hand positions
  handWorldPoses <- flip handsToWorldPoses hands . transformationFromPose <$> use (wldPlayer . plrPose)
  wldPlayer . plrHandPoses .= map poseFromMatrix handWorldPoses

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
      --applyHandJoystickMovement hands (wldPlayer . plrPose)
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
        addCube transceiver (shiftBy (V3 0 0.1 0) playerPose)
    
    -- Handle key events
    -- Spawn a cube offset by 0.1 y
    onKeyDown Key'Space e (addCube transceiver (shiftBy (V3 0 0.1 0) playerPose))
    onKeyDown Key'F e (setCursorInputMode gpWindow CursorInputMode'Disabled)
    onKeyDown Key'G e (setCursorInputMode gpWindow CursorInputMode'Normal)
    onKeyDown Key'O e (recenterWhenOculus gamePal)
    onKeyDown Key'Z e (addCube transceiver newPose)
    onKeyDown Key'N e startLogo
    onKeyDown Key'M e startMain
    onKeyDown Key'0 e (restart transceiver)

  xDown <- (== KeyState'Pressed) <$> getKey gpWindow Key'X
  -- Til I finish per-hand vacuuming, vacuum when either bumper is down
  let shouldVacuum = or (map (^. hndTrigger . to (> 0.5)) hands) || xDown
  wldPlayer . plrVacuum .= shouldVacuum

  -- Fire cubes from each hand when their triggers are held down
  forM_ (zip hands handWorldPoses) $ \(hand, handMatrix) -> do

    -- Bind Hydra 'Start' buttons to HMD Recenter
    when (hand ^. hndButtonS) $ recenterWhenOculus gamePal

    processHandCubeFiring hand (poseFromMatrix handMatrix) frameNumber transceiver

recenterWhenOculus gamePal = case gpHMD gamePal of
  OculusHMD hmd -> liftIO $ recenterPose hmd
  _ -> return ()

processHandCubeFiring :: (Integral a, MonadIO m, MonadState World m, MonadRandom m) 
                      => Hand -> Pose GLfloat -> a -> Transceiver Op -> m ()    
processHandCubeFiring hand handPose frameNumber transceiver  = do
  let triggerIsDown = hand ^. hndTrigger > 0.5
  triggerWasDown <- fromMaybe False <$> use (wldHandTriggers . at (hand ^. hndID))
  wldHandTriggers . at (hand ^. hndID) ?= triggerIsDown

  phase <- use wldPhase
  --printIO triggerIsDown

  case phase of
    PhaseVoid -> when triggerIsDown $ startLogo
    PhaseLogo -> return ()
    _ -> do
      -- Move the cube upwards a bit so it spawns at the tip of the hand
      let cubePose = shiftBy (V3 0 0 (-0.5)) handPose
      -- Spawn every 0.1 secs, and on fresh trigger squeezes
          shouldSpawn = triggerIsDown && not triggerWasDown 
                        -- || triggerIsDown && frameNumber `mod` 30 == 0
      when shouldSpawn $
        addCube transceiver cubePose


addCube :: (MonadIO m, MonadState World m, MonadRandom m) => Transceiver Op -> Pose GLfloat -> m ()
addCube transceiver pose = do
  -- Spawn a cube at the player's position and orientation
  instruction <- newCubeInstruction pose

  interpret instruction

  writeTransceiver transceiver $ Reliable instruction
  return ()


startLogo :: (MonadState World m, MonadIO m) => m ()
startLogo = do
  wldPhase .= PhaseLogo
  wldTime  .= 0
  sendGlobal "startLogo" Bang

startMain :: (MonadState World m) => m ()
startMain = do
  wldPhase .= PhaseMain
  wldTime  .= 0

restart :: (MonadState World m, MonadIO m) => Transceiver Op -> m ()
restart transceiver = do
  writeTransceiver transceiver $ Reliable Restart
  wldStarted .= 0
  wldPhase .= PhaseVoid
  wldTime  .= 0






