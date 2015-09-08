{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts, LambdaCase #-}

module Controls where

import Graphics.UI.GLFW.Pal

import Graphics.Oculus
import Linear

import System.Hardware.Hydra

import Control.Monad
import Control.Monad.State.Strict
import Control.Lens hiding (view)
import Control.Monad.Random

import Network.UDP.Pal
import Types

import Game.Pal

processControls :: (MonadIO m, MonadState World m, MonadRandom m) 
                => GamePal
                -> Transceiver Op
                -> Integer
                -> m ()
processControls GamePal{..} transceiver frameNumber = do
  -- Get latest Hydra data
  hands <- maybe (return []) getHands gpSixenseBase

  -- Update hand positions
  handWorldPoses <- handsToWorldPoses hands <$> use (wldPlayer . plrPose)
  wldPlayer . plrHandPoses .= handWorldPoses

  -- Update head position
  wldPlayer . plrHeadPose <~ getMaybeHMDPose gpHMD

  -- Handle Hydra movement events, or mouse if no Hydra present
  if null hands 
    then do
      -- Disabled mouselook because it's dumb
      -- isFocused <- getWindowFocused window
      -- when isFocused $ applyMouseLook window (wldPlayer . plrPose)
      return ()
    else do
      -- Disabled hydra joysticks for no motion sickness
      --applyHydraJoystickMovement hands (wldPlayer . plrPose)
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
    onKeyDown Key'E e (addCube transceiver (shiftBy (V3 0 0.1 0) playerPose))
    onKeyDown Key'F e (setCursorInputMode gpWindow CursorInputMode'Disabled)
    onKeyDown Key'G e (setCursorInputMode gpWindow CursorInputMode'Normal)
    onKeyDown Key'O e (maybe (return ()) (liftIO . recenterPose) gpHMD)

  -- Fire cubes from each hand when their triggers are held down
  forM_ (zip hands handWorldPoses) $ \(handData, handPose) -> do
    -- Move the cube upwards a bit so it spawns at the tip of the hand
    let cubePose = shiftBy (V3 0 0.5 0) handPose
    when (trigger handData > 0.5 && frameNumber `mod` 30 == 0) $ 
      addCube transceiver cubePose




addCube :: (MonadIO m, MonadState World m, MonadRandom m) => Transceiver Op -> Pose -> m ()
addCube transceiver pose = do
  -- Spawn a cube at the player's position and orientation
  instruction <- do
    objID <- getRandom
    return $ CreateObject objID (Object pose 0.25)

  interpret instruction

  writeTransceiver transceiver $ Reliable instruction
  return ()
