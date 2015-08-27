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
                => Window
                -> Events
                -> Maybe SixenseBase
                -> Maybe HMD
                -> Transceiver Op
                -> Integer
                -> m ()
processControls window events sixenseBase maybeHMD transceiver frameNumber = do
  -- Get latest Hydra data
  hands <- maybe (return []) getHands sixenseBase

  -- Update hand positions
  handWorldPoses <- handsToWorldPoses hands <$> use (wldPlayer . plrPose)
  wldPlayer . plrHandPoses .= handWorldPoses

  -- Update head position
  wldPlayer . plrHeadPose <~ getMaybeHMDPose maybeHMD

  -- Handle Hydra movement events, or mouse if no Hydra present
  if null hands 
    then do
      -- isFocused <- getWindowFocused window
      -- when isFocused $ applyMouseLook window (wldPlayer . plrPose)
      return ()
    else do
      applyHydraJoystickMovement hands (wldPlayer . plrPose)
      return ()
  -- Handle keyboard movement events
  applyWASD window (wldPlayer . plrPose)
  

  -- Handle UI events
  processEvents events $ \e -> do
    closeOnEscape window e

    applyGamepadJoystickMovement e (wldPlayer . plrPose)

    -- Get player pose
    Pose playerPos playerRot <- use (wldPlayer . plrPose)

    onGamepadAxes e $ \GamepadAllAxes{..} -> 
      -- Use the right trigger to fire a cube
      when (gaxTriggers < (-0.5) && frameNumber `mod` 30 == 0) $ 
        addCube transceiver (Pose (rotate playerRot (V3 0 0.1 0) + playerPos) playerRot)
    
    -- Handle key events
    -- Spawn a cube offset by 0.1 y
    onKeyDown Key'E e (addCube transceiver (Pose (rotate playerRot (V3 0 0.1 0) + playerPos) playerRot))
    onKeyDown Key'F e (setCursorInputMode window CursorInputMode'Disabled)
    onKeyDown Key'G e (setCursorInputMode window CursorInputMode'Normal)
    onKeyDown Key'O e (maybe (return ()) (liftIO . recenterPose) maybeHMD)

  -- Fire cubes from each hand when their triggers are held down
  forM_ (zip hands handWorldPoses) $ \(handData, Pose handPos handRot) -> do
    -- Move the cube upwards a bit so it spawns at the tip of the hand
    let cubePose = Pose (rotate handRot (V3 0 0.5 0) + handPos) handRot
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
