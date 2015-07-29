{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts, LambdaCase #-}

module Controls where

import Graphics.UI.GLFW.Pal

import Graphics.GL
import Graphics.Oculus
import Linear

import System.Hardware.Hydra

import Control.Monad
import Control.Monad.State
import Control.Lens hiding (view)
import Control.Monad.Random

import Network.UDP.Pal
import Types
import Movement

processControls :: (MonadState World m, MonadIO m, MonadRandom m) =>
                         Window
                         -> Events
                         -> Maybe SixenseBase
                         -> Maybe HMD
                         -> Transceiver Op
                         -> Integer
                         -> m ()
processControls window events sixenseBase maybeHMD transceiver frameNumber = do
  -- Get latest Hydra data
  hands <- maybe (return []) getHands sixenseBase

  -- Handle Hydra movement events, or mouse if no Hydra present
  case hands of
    [left, right] -> do
      -- Move player forward/back/left/right with left joystick
      movePlayer (V3 (joystickX left / 10) 0 (-(joystickY left / 10)))
      -- Quat rotation must be rotation * original rather than vice versa
      wldPlayer . plrPose . posOrientation %= \old -> axisAngle ( V3 0 1 0 ) (-joystickX right * 0.1) * old
      -- Move player down and up with left and right joystick clicks
      when (ButtonJoystick `elem` handButtons left)  $ movePlayer ( V3 0 (-0.1) 0  )
      when (ButtonJoystick `elem` handButtons right) $ movePlayer ( V3 0   0.1  0  )
    _ -> do
      -- Handle mouse events
      isFocused <- getWindowFocused window
      when isFocused $ applyMouseLook window
  
  -- Handle keyboard movement events
  applyMovement window

  -- Get player pose
  playerPos <- use (wldPlayer . plrPose . posPosition)
  playerRot <- use (wldPlayer . plrPose . posOrientation)

  -- Handle UI events
  processEvents events $ \e -> do
    closeOnEscape window e

    case e of
      GamepadAxes GamepadAllAxes{..} -> do
        movePlayer (V3 (realToFrac gaxLeftStickX / 10) 0 (realToFrac gaxLeftStickY / 10))
        -- Quat rotation must be rotation * original rather than vice versa
        wldPlayer . plrPose . posOrientation %= \old -> axisAngle ( V3 0 1 0 ) (-(realToFrac gaxRightStickY) * 0.1) * old

        -- Use the right trigger to fire a cube
        when (gaxTriggers < (-0.5) && frameNumber `mod` 30 == 0) $ 
          addCube transceiver (rotate playerRot (V3 0 0.1 0) + playerPos) playerRot
      _ -> return ()
    -- Handle key events
    -- Spawn a cube offset by 0.1 y
    keyDown Key'E e (addCube transceiver (rotate playerRot (V3 0 0.1 0) + playerPos) playerRot)
    keyDown Key'F e (setCursorInputMode window CursorInputMode'Disabled)
    keyDown Key'G e (setCursorInputMode window CursorInputMode'Normal)

    keyDown Key'Y e ( liftIO . print =<< use wldEyeDebug )

  -- Update hand positions
  
  let handWorldPoses = map handWorldPose hands
      handWorldPose handData = Pose positWorld orientWorld
        where
          handPosit   = fmap (realToFrac . (/500)) (pos handData) + V3 0 (-1) (-1)
          handOrient  = rotQuat handData
          positWorld  = rotate playerRot handPosit + playerPos
          orientWorld = playerRot * handOrient

  wldPlayer . plrHandPoses .= handWorldPoses

  -- Update head position
  (headOrient, headPosit) <- maybe (return (axisAngle (V3 0 1 0) 0, V3 0 0 0)) (liftIO . getHMDPose) maybeHMD
  wldPlayer . plrHeadPose .= Pose headPosit headOrient
  

  -- Fire cubes from each hand when their triggers are held down
  forM_ (zip hands handWorldPoses) $ \(handData, Pose posit orient) -> do
    when (trigger handData > 0.5 && frameNumber `mod` 30 == 0) $ 
      addCube transceiver posit orient



addCube :: (MonadIO m, MonadState World m, MonadRandom m) => Transceiver Op -> V3 GLfloat -> Quaternion GLfloat -> m ()
addCube transceiver posit orient = do
  -- Spawn a cube at the player's position and orientation
  instruction <- do
    objID <- getRandom
    return $ UpdateObject objID (Object posit orient 0.25)
  putStrLnIO "Print creating a cube sir"
  
  interpret instruction


  writeTransceiver transceiver $ Reliable instruction
  return ()
