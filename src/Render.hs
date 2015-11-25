{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts, LambdaCase #-}

module Render where

import Graphics.GL

import Linear.Extra

import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Lens.Extra
import qualified Data.Map.Strict as Map
import Data.Maybe

import Graphics.GL.Pal

import Types
import Data.Monoid
import Data.Foldable

import Animation.Pal


listToTuple :: (t, t) -> [t] -> (t, t)
listToTuple _   [a,b] = (a,b)
listToTuple def _     = def

-- | Extracts a list of results using the given lens
useListOf :: MonadState s m => Getting (Endo [a]) s a -> m [a]
useListOf aLens = get <&> (^.. aLens)

viewListOf :: MonadReader s m => Getting (Endo [a]) s a -> m [a]
viewListOf aLens = ask <&> (^.. aLens)

getLocalHandPositions :: MonadReader World m => m [V3 GLfloat]
getLocalHandPositions = 
  viewListOf ( wldPlayer 
             . plrHandPoses 
             . traverse 
             . to (shiftBy handLightOffset) 
             . posPosition 
             )


-- | We draw the second pair of lights for one remote player
-- (4 is the most we can handle currently)
-- Defaults to 0,0 if no remote players... 
-- should we remove the lights instead?
-- (e.g. default to infinity,infinity)
getFirstRemoteHandPositions :: MonadReader World m => m [V3 GLfloat]
getFirstRemoteHandPositions = do
  remoteHandPoses <- listToMaybe <$> viewListOf (wldPlayers . traverse . plrHandPoses)
  return $ 
    case remoteHandPoses of
      Just hands -> hands ^.. traverse . posPosition
      Nothing    -> []


--render :: (MonadIO m, MonadReader World m) 
--       => Shapes
--       -> M44 GLfloat
--       -> M44 GLfloat
--       -> m ()
render Shapes{..} players cubes lights filledness projection viewMat = do

  let projectionView = projection !*! viewMat
      eyePos = fromMaybe viewMat (inv44 viewMat) ^. translation

  --drawLights  light      projectionView        lights filledness
  drawPlayers hand handle face  projectionView eyePos lights players

  phase <- view wldPhase
  case phase of 
    PhaseVoid ->
      drawLogo logo projectionView eyePos lights
    PhaseLogo ->
      drawLogo logo projectionView eyePos lights
    PhaseMain ->
      drawCubes cube projectionView eyePos lights cubes filledness 
    PhaseEnd  ->
      drawCubes logo  projectionView eyePos lights cubes filledness

  drawRoom room projectionView eyePos lights filledness

  
--drawCubes :: (MonadIO m, MonadReader World m)
--          => Shape Uniforms
--          -> M44 GLfloat
--          -> V3 GLfloat
--          -> [V3 GLfloat]
--          -> GLfloat
--          -> m ()
drawCubes cube projectionView eyePos lights cubes filledness = do
  

  let Uniforms{..} = sUniforms cube
  useProgram (sProgram cube)

  uniformF  uTime =<< view wldTime
  uniformF  uStarted =<< view wldStarted
  uniformF  uDayNight =<< dayNightCycleAt <$> view wldTime
  uniformF  uDayLength dayLength
  
  uniformV3 uCamera eyePos


  setLightUniforms cube lights

  withVAO (sVAO cube) $ do

    glDisable GL_CULL_FACE
    glCullFace GL_BACK
    
    forM_ (zip [0..] (Map.toList cubes)) $ \(i , (objID, obj)) -> do

      mVoiceID <- view (wldCubeVoices . at objID)
      (pitch, amp) <- case mVoiceID of
        Just voiceID -> do
          pitch <- fromMaybe 0 <$> view (wldVoicePitch . at voiceID)
          amp   <- fromMaybe 0 <$> view (wldVoiceAmplitude . at voiceID)
          return (pitch, amp)
        Nothing      -> return (0, 0)
      uniformV3 uParameterA (V3 pitch amp 0)

      -- mCollision <- view (wldLastCollisions . at objID)
      -- forM_ mCollision $ \collision -> do

      --   -- TODO(isaac) fill in uniforms for collision here:
      --   uniformF  uCollisionTime      (collision ^. ccTime)
      --   -- uniformF  uCollisionImpulse   (collision ^. ccImpulse)
      --   uniformV3 uCollisionPosition  (collision ^. ccPosition)
      --   -- uniformV3 uCollisionDirection (collision ^. ccDirection)
      --   return ()
      

      let rotateVec = rotate (obj ^. objPose . posOrientation) (V3 0 0 1) 
      
      uniformV3 uParameterB rotateVec

      uniformF uFilledness filledness
      -- uniformF uComplete   =<< view wldComplete

      let model = transformationFromPose (obj ^. objPose)
          -- The server will continuously update us on the scale of the cube
          scaledModel = model !*! scaleMatrix (realToFrac (obj ^. objScale))
      drawShape' scaledModel projectionView i cube


drawLogo :: (MonadIO m, MonadReader World m)
          => Shape Uniforms
          -> M44 GLfloat
          -> V3 GLfloat
          -> [V3 GLfloat]
          -> m ()
drawLogo cube projectionView eyePos lights = do


  let Uniforms{..} = sUniforms cube
  useProgram (sProgram cube)

  uniformF  uTime =<< view wldTime

  uniformV3 uCamera eyePos

  setLightUniforms cube lights

  withVAO (sVAO cube) $ do

    glDisable GL_CULL_FACE
    glCullFace GL_BACK
    
    let obj = logoObject
        model = transformationFromPose (obj ^. objPose)
        scaledModel = model !*! scaleMatrix (realToFrac (obj ^. objScale))

    drawShape' scaledModel projectionView 0 cube


setLightUniforms :: (MonadIO m) 
                 => Shape Uniforms
                 -> [V3 GLfloat]
                 -> m ()
setLightUniforms anShape lights = do
  let Uniforms{..} = sUniforms anShape
      uniformsAndPositions = zip [uLight1, uLight2, uLight3, uLight4] lights
  forM_ uniformsAndPositions $ \(lightUniform, lightPos) ->
    uniformV3 lightUniform lightPos


drawLights :: (MonadIO m, MonadReader World m) 
           => Shape Uniforms
           -> M44 GLfloat
           -> [V3 GLfloat]
           -> GLfloat
           -> m ()
drawLights anShape projectionView lights filledness = do
  let Uniforms{..} = sUniforms anShape
  useProgram (sProgram anShape)

  uniformF  uTime =<< view wldTime
  uniformF  uDayNight =<< dayNightCycleAt <$> view wldTime
  uniformF  uDayLength dayLength



  withVAO (sVAO anShape) $ do

    glEnable GL_CULL_FACE
    glCullFace GL_BACK

    forM_ (zip [0..] lights) $ \(i, lightPos) -> do
      let model = mkTransformation (axisAngle (V3 1 0 0) 0.0) lightPos
      -- printIO (i, lightPos)
      
      uniformV3 uParameterA lightPos

      uniformF uFilledness filledness
      -- uniformF uComplete   =<< view wldComplete
      
      drawShape' model projectionView i anShape


--drawPlayers :: (MonadIO m, MonadReader World m) 
--            => Shape Uniforms
--            -> Shape Uniforms
--            -> Shape Uniforms
--            -> M44 GLfloat
--            -> V3 GLfloat
--            -> [V3 GLfloat]
--            -> m ()
drawPlayers hand handle face projectionView eyePos lights players = do

  useProgram (sProgram hand)
  let Uniforms{..} = sUniforms hand

  uniformV3 uCamera eyePos
  uniformF  uTime =<< view wldTime
  uniformF  uDayNight =<< dayNightCycleAt <$> view wldTime
  uniformF  uDayLength dayLength

  setLightUniforms hand lights

  withVAO (sVAO hand) $ do

    glEnable GL_CULL_FACE
    glCullFace GL_BACK

    drawLocalHands  projectionView hand
    drawRemoteHands projectionView hand players

  drawHandles projectionView eyePos handle lights players

  drawRemoteHeads projectionView eyePos face lights players
  -- DEBUG (display local head shifted in space so it can be observed)
  -- localPlayer <- view wldPlayer
  -- let localPlayer' = localPlayer & plrPose %~ shiftBy (V3 (-1) 0 0)
  -- drawRemoteHeads projectionView eyePos face lights (localPlayer':players)
  -- /DEBUG

drawHandles projectionView eyePos handle lights players = do
  useProgram (sProgram handle)
  let Uniforms{..} = sUniforms handle

  uniformV3 uCamera eyePos
  uniformF  uTime =<< view wldTime
  uniformF  uDayNight =<< dayNightCycleAt <$> view wldTime
  uniformF  uDayLength dayLength

  setLightUniforms handle lights

  withVAO (sVAO handle) $ do

    glEnable GL_CULL_FACE
    glCullFace GL_BACK

    drawLocalHandles  projectionView handle
    drawRemoteHandles projectionView handle players

drawLocalHands :: (MonadIO m, MonadReader World m) 
               => M44 GLfloat -> Shape Uniforms -> m ()
drawLocalHands projectionView hand = do

  let Uniforms{..} = sUniforms hand

  uniformF  uStarted =<< view wldStarted


  -- Draw the local player's hands
  handPoses <- view $ wldPlayer . plrHandPoses
  forM_ handPoses $ \handPose -> do

    let finalMatrix = transformationFromPose $ shiftBy handOffset handPose
        rotateVec = rotate (handPose ^. posOrientation) (V3 0 0 1)

    uniformV3 uParameterA $ handPose ^. posPosition
    uniformV3 uParameterB $ rotateVec

    drawShape' finalMatrix projectionView 0 hand


drawRemoteHands :: (MonadIO m, MonadReader World m) 
                => M44 GLfloat -> Shape Uniforms -> [Player] -> m ()
drawRemoteHands projectionView hand players = do
  let Uniforms{..} = sUniforms hand
   
  uniformF  uStarted =<< view wldStarted
  uniformF  uDayNight =<< dayNightCycleAt <$> view wldTime
  uniformF  uDayLength dayLength

  forM_ players $ \player -> 
    forM_ (player ^. plrHandPoses) $ \handPose -> do

      let finalMatrix = transformationFromPose $ shiftBy handOffset handPose
          rotateVec = rotate (handPose ^. posOrientation) (V3 0 0 1) 
      
      uniformV3 uParameterA $ handPose ^. posPosition
      uniformV3 uParameterB rotateVec
      
      drawShape' finalMatrix projectionView 0 hand


-- drawing handles is the same as drawing the hands
-- but with no offeset, since they will be right on the actual controllers
drawLocalHandles :: (MonadIO m, MonadReader World m) 
               => M44 GLfloat -> Shape Uniforms -> m ()
drawLocalHandles projectionView hand = do

  let Uniforms{..} = sUniforms hand

  uniformF  uStarted =<< view wldStarted


  -- Draw the local player's hands
  handPoses <- view $ wldPlayer . plrHandPoses
  forM_ handPoses $ \handPose -> do

    let finalMatrix = transformationFromPose handPose
        rotateVec = rotate (handPose ^. posOrientation) (V3 0 0 1)

    uniformV3 uParameterA $ handPose ^. posPosition
    uniformV3 uParameterB $ rotateVec

    drawShape' finalMatrix projectionView 0 hand




drawRemoteHandles :: (MonadIO m, MonadReader World m) 
                => M44 GLfloat -> Shape Uniforms -> [Player] -> m ()
drawRemoteHandles projectionView hand players = do
  let Uniforms{..} = sUniforms hand
   
  uniformF  uStarted =<< view wldStarted
  uniformF  uDayNight =<< dayNightCycleAt <$> view wldTime
  uniformF  uDayLength dayLength

  forM_ players $ \player -> 
    forM_ (player ^. plrHandPoses) $ \handPose -> do

      let finalMatrix = transformationFromPose handPose
          rotateVec = rotate (handPose ^. posOrientation) (V3 0 0 1) 
      
      uniformV3 uParameterA $ handPose ^. posPosition
      uniformV3 uParameterB rotateVec
      
      drawShape' finalMatrix projectionView 0 hand


drawRemoteHeads :: (MonadIO m, MonadReader World m) 
                => M44 GLfloat
                -> V3 GLfloat
                -> Shape Uniforms
                -> [V3 GLfloat]
                -> [Player]
                -> m ()
drawRemoteHeads projectionView eyePos face lights players = do
  let Uniforms{..} = sUniforms face
  -- Draw all remote players' heads 
  -- (we don't draw the local player's head)
  useProgram (sProgram face)
  uniformV3 uCamera eyePos
  uniformF  uTime =<< view wldTime
  uniformF  uDayNight =<< dayNightCycleAt <$> view wldTime
  uniformF  uDayLength dayLength
  
  setLightUniforms face lights

  withVAO (sVAO face) $ do
    forM_ players $ \player -> do
      let finalMatrix = transformationFromPose (totalHeadPose player)

      drawShape' finalMatrix projectionView 0 face


--drawRoom :: (MonadIO m, MonadReader World m) 
--         => Shape Uniforms
--         -> M44 GLfloat
--         -> V3 GLfloat
--         -> [V3 GLfloat]
--         -> GLfloat
--         -> m ()
drawRoom room projectionView eyePos lights filledness = do
  let Uniforms{..} = sUniforms room

  useProgram (sProgram room)

  uniformF  uTime =<< view wldTime
  uniformF  uStarted =<< view wldStarted
  uniformF  uDayNight =<< dayNightCycleAt <$> view wldTime
  uniformF  uDayLength dayLength

  player <- view wldPlayer 

  let Pose totalHeadPosition totalHeadOriention = totalHeadPose player
  
  let rotateVec = rotate totalHeadOriention (V3 0 0 1)   

  uniformV3 uParameterA totalHeadPosition
  uniformV3 uParameterB rotateVec

  uniformF uFilledness filledness
  -- uniformF uComplete   =<< view wldComplete

  kickVoiceID <- view wldKickVoiceID
  tick <- fromMaybe 0 <$> view (wldVoicePitch . at kickVoiceID)
  uniformF uTick tick

  uniformV3 uCamera eyePos

  setLightUniforms room lights
  
  withVAO (sVAO room) $ do

    glEnable GL_CULL_FACE
    glCullFace GL_FRONT

    let model = identity

    drawShape' model projectionView 0 room

drawShape' :: MonadIO m => M44 GLfloat -> M44 GLfloat -> GLfloat -> Shape Uniforms -> m ()
drawShape' model projectionView drawID shape = do 

  let Uniforms{..} = sUniforms shape

  uniformM44 uModelViewProjection (projectionView !*! model)
  uniformM44 uInverseModel        (fromMaybe model (inv44 model))
  uniformM44 uModel model

  uniformF uID drawID

  let vc = geoVertCount (sGeometry shape) 
  glDrawElements GL_TRIANGLES vc GL_UNSIGNED_INT nullPtr


