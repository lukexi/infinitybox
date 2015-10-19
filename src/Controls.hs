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

import Game.Pal
import Data.Maybe
import Sound.Pd1

import Control.Concurrent


processControls :: (MonadIO m, MonadState World m, MonadRandom m) 
                => GamePal
                -> MVar (Transceiver Op)
                -> Integer
                -> m ()
processControls gamePal@GamePal{..} transceiverMVar frameNumber = do
  -- Get latest Hydra data
  (hands, handsType) <- getHands gamePal

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
        addCube transceiverMVar (shiftBy (V3 0 0.1 0) playerPose)
    
    -- Handle key events
    -- Spawn a cube offset by 0.1 y
    onKeyDown Key'Space e (addCube transceiverMVar (shiftBy (V3 0 0.1 0) playerPose))
    onKeyDown Key'F e (setCursorInputMode gpWindow CursorInputMode'Disabled)
    onKeyDown Key'G e (setCursorInputMode gpWindow CursorInputMode'Normal)
    onKeyDown Key'O e (recenterWhenOculus gamePal)
    onKeyDown Key'Z e (addCube transceiverMVar newPose)
    onKeyDown Key'N e startLogo
    onKeyDown Key'M e startMain
    onKeyDown Key'0 e (restart transceiverMVar)

  xDown <- (== KeyState'Pressed) <$> getKey gpWindow Key'X

  -- Til I finish per-hand vacuuming, vacuum when either bumper is down
  let shouldVacuum = or (map (^. hndGrip . to (> 0.5)) hands) || xDown
  wldPlayer . plrVacuum .= shouldVacuum

  -- Fire cubes from each hand when their triggers are held down
  forM_ (zip hands handWorldPoses) $ \(hand, handMatrix) -> do

    -- Bind Hydra 'Start' buttons to HMD Recenter
    when (hand ^. hndButtonS) $ recenterWhenOculus gamePal

    processHandCubeFiring hand (poseFromMatrix handMatrix) frameNumber transceiverMVar
  wldLastPlayer <~ use wldPlayer



processHandCubeFiring :: (Integral a, MonadIO m, MonadState World m, MonadRandom m) 
                      => Hand -> Pose GLfloat -> a -> MVar (Transceiver Op) -> m ()    
processHandCubeFiring hand handPose _frameNumber transceiverMVar  = do

  -- Determine if the trigger is freshly pressed
  triggerWasDown <- fromMaybe False <$> use (wldLastPlayer . plrHandPoses . at (hand ^. hndID))
  let triggerIsDown = hand ^. hndTrigger > 0.5
      isNewTrigger  = triggerIsDown && not triggerWasDown

  -- Determine if the start button is freshly pressed
  -- Sample the current player position and write it as a dummy remote player
  -- to allow inspecting what a multiplayer person looks like
  startWasDown <- fromMaybe False <$> use (wldLastPlayer . plrHandPoses . at (hand ^. hndID))
  when (not startWasDown && hand ^. hndButtonS) $ do
    player <- use wldPlayer
    wldPlayers . at 1000 ?= player

  phase <- use wldPhase

  case phase of
    PhaseVoid -> when triggerIsDown $ startLogo
    PhaseLogo -> return ()
    _ -> do
      -- Move the cube upwards a bit so it spawns at the tip of the hand
      let cubePose = shiftBy (V3 0 0 (-0.5)) handPose
      -- Spawn every 0.1 secs, and on fresh trigger squeezes
          shouldSpawn = isNewTrigger
                        -- || triggerIsDown && frameNumber `mod` 30 == 0
      when shouldSpawn $
        addCube transceiverMVar cubePose


addCube :: (MonadIO m, MonadState World m, MonadRandom m) => MVar (Transceiver Op) -> Pose GLfloat -> m ()
addCube transceiverMVar pose = do
  -- Spawn a cube at the player's position and orientation
  instruction <- newCubeInstruction pose

  interpret instruction

  whenMVar transceiverMVar $ \transceiver -> 
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
