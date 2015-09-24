{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts, LambdaCase #-}

module Render where

import Graphics.GL

import Linear.Extra

import Control.Monad
import Control.Monad.State.Strict
import Control.Lens.Extra
import qualified Data.Map.Strict as Map
import Data.Maybe

import Graphics.GL.Pal

import Types
import Data.Monoid

import Animation.Pal


listToTuple :: (t, t) -> [t] -> (t, t)
listToTuple _   [a,b] = (a,b)
listToTuple def _     = def

useListOf :: MonadState s m => Getting (Endo [a]) s a -> m [a]
useListOf aLens = do
  stat <- use id
  return (stat ^.. aLens)

getLocalHandPositions :: MonadState World m => m [V3 GLfloat]
getLocalHandPositions = 
  useListOf ( wldPlayer 
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
getFirstRemoteHandPositions :: MonadState World m => m [V3 GLfloat]
getFirstRemoteHandPositions = do
  remoteHandPoses <- listToMaybe <$> useListOf (wldPlayers . traverse . plrHandPoses)
  return $ 
    case remoteHandPoses of
      Just hands -> hands ^.. traverse . posPosition
      Nothing -> []

render :: (MonadIO m, MonadState World m) 
       => Resources
       -> M44 GLfloat
       -> M44 GLfloat
       -> m ()
render Resources{..} projection viewMat = do

  let projectionView = projection !*! viewMat
      eyePos = fromMaybe viewMat (inv44 viewMat) ^. translation

  lights12 <- getLocalHandPositions
  lights34 <- getFirstRemoteHandPositions
  let lightPositions = lights12 ++ lights34

  fillednessAnim  <- use wldFilledness
  now <- getNow
  let filledness = evanResult (evalAnim now fillednessAnim)

  --let theme =

  drawLights  light      projectionView        lightPositions filledness
  drawPlayers hand face  projectionView eyePos lightPositions 

  phase <- use wldPhase
  case phase of 
    PhaseVoid ->
      drawLogo logo projectionView eyePos lightPositions
    PhaseLogo ->
      drawLogo logo projectionView eyePos lightPositions
    PhaseMain ->
      drawCubes cube projectionView eyePos lightPositions filledness
    PhaseEnd  ->
      drawCubes logo  projectionView eyePos lightPositions filledness

  drawRoom room projectionView eyePos lightPositions filledness


  


  
drawCubes :: (MonadIO m, MonadState World m)
          => Shape Uniforms
          -> M44 GLfloat
          -> V3 GLfloat
          -> [V3 GLfloat]
          -> GLfloat
          -> m ()
drawCubes cube projectionView eyePos lights filledness = do
  -- Interpolate between the last and newest cube states
  newCubes  <- use wldCubes
  lastCubes <- use wldLastCubes
  let cubes = Map.unionWith interpolateObjects lastCubes newCubes

  let Uniforms{..} = sUniforms cube
  useProgram (sProgram cube)

  uniformF  uTime =<< use wldTime
  uniformF  uStarted =<< use wldStarted
  uniformF  uDayNight =<< dayNightCycleAt <$> use wldTime
  uniformF  uDayLength dayLength
  -- putStrLnIO (show view)
  
  uniformV3 uCamera eyePos


  setLightUniforms cube lights

  withVAO (sVAO cube) $ do

    glDisable GL_CULL_FACE
    glCullFace GL_BACK
    
    forM_ ( zip [0..] ( Map.toList cubes ) ) $ \( i , (objID, obj) ) -> do


      mVoiceID <- use (wldCubeVoices . at objID)
      (pitch, amp) <- case mVoiceID of
        Just voiceID -> do
          pitch <- fromMaybe 0 <$> use (wldVoicePitch . at voiceID)
          amp   <- fromMaybe 0 <$> use (wldVoiceAmplitude . at voiceID)
          return (pitch, amp)
        Nothing      -> return (0, 0)
      uniformV3 uParameterA (V3 pitch amp 0)

      -- mCollision <- use (wldLastCollisions . at objID)
      -- forM_ mCollision $ \collision -> do

      --   -- TODO(isaac) fill in uniforms for collision here:
      --   uniformF  uCollisionTime      (collision ^. ccTime)
      --   -- uniformF  uCollisionImpulse   (collision ^. ccImpulse)
      --   uniformV3 uCollisionPosition  ( collision ^. ccPosition  ) 
      --   -- uniformV3 uCollisionDirection (collision ^. ccDirection)
      --   return ()
      

      let rotateVec = rotate (obj ^. objPose . posOrientation) (V3 0 0 1) 
      
      uniformV3 uParameterB rotateVec

      uniformF uFilledness filledness
      -- uniformF uComplete   =<< use wldComplete

      cubeAge <- min 1 . fromMaybe 0 <$> use (wldCubeAges . at objID)
      let model = transformationFromPose (obj ^. objPose)
          -- Scale the cube up from 0 at beginning. 
          -- This is render-only at the moment; physics object is still
          -- full size from the beginning.
          -- scaledModel = model !*! scaleMatrix 0.3
          scaledModel = model !*! scaleMatrix (realToFrac ( cubeAge * ( obj ^. objScale ))) 
      drawShape scaledModel projectionView i cube

drawLogo :: (MonadIO m, MonadState World m)
          => Shape Uniforms
          -> M44 GLfloat
          -> V3 GLfloat
          -> [V3 GLfloat]
          -> m ()
drawLogo cube projectionView eyePos lights = do


  let Uniforms{..} = sUniforms cube
  useProgram (sProgram cube)

  uniformF  uTime =<< use wldTime

  uniformV3 uCamera eyePos

  setLightUniforms cube lights

  withVAO (sVAO cube) $ do

    glDisable GL_CULL_FACE
    glCullFace GL_BACK
    
    let obj = logoObject
        model = transformationFromPose (obj ^. objPose)
        scaledModel = model !*! scaleMatrix ( realToFrac (obj ^. objScale) )

    drawShape scaledModel projectionView 0 cube



setLightUniforms :: (MonadIO m) 
                 => Shape Uniforms
                 -> [V3 GLfloat]
                 -> m ()
setLightUniforms anShape lights = do
  let Uniforms{..} = sUniforms anShape
      uniformsAndPositions = zip [uLight1, uLight2, uLight3, uLight4] lights
  forM_ uniformsAndPositions $ \(lightUniform, lightPos) ->
    uniformV3 lightUniform lightPos

drawLights :: (MonadIO m , MonadState World m) 
           => Shape Uniforms
           -> M44 GLfloat
           -> [V3 GLfloat]
           -> GLfloat
           -> m ()
drawLights anShape projectionView lights filledness = do
  let Uniforms{..} = sUniforms anShape
  useProgram (sProgram anShape)

  uniformF  uTime =<< use wldTime
  uniformF  uDayNight =<< dayNightCycleAt <$> use wldTime
  uniformF  uDayLength dayLength



  withVAO (sVAO anShape) $ do

    glEnable GL_CULL_FACE
    glCullFace GL_BACK

    forM_ (zip [0..] lights) $ \(i, lightPos) -> do
      let model = mkTransformation (axisAngle (V3 1 0 0) 0.0) lightPos
      -- printIO (i, lightPos)
      
      uniformV3 uParameterA lightPos

      uniformF uFilledness filledness
      -- uniformF uComplete   =<< use wldComplete

      
      drawShape model projectionView i anShape

drawPlayers :: (MonadIO m, MonadState World m) 
            => Shape Uniforms
            -> Shape Uniforms
            -> M44 GLfloat
            -> V3 GLfloat
            -> [V3 GLfloat]
            -> m ()
drawPlayers hand face projectionView eyePos lights = do
  useProgram (sProgram hand)
  let Uniforms{..} = sUniforms hand
  uniformV3 uCamera eyePos
  uniformF  uTime =<< use wldTime
  uniformF  uDayNight =<< dayNightCycleAt <$> use wldTime
  uniformF  uDayLength dayLength

  setLightUniforms hand lights

  withVAO (sVAO hand) $ do

    glEnable GL_CULL_FACE
    glCullFace GL_BACK

    drawLocalHands projectionView hand
    drawRemoteHands projectionView hand

  drawRemoteHeads projectionView eyePos face lights

drawLocalHands :: (MonadIO m, MonadState World m) 
               => M44 GLfloat -> Shape Uniforms -> m ()
drawLocalHands projectionView hand = do
  let Uniforms{..} = sUniforms hand

  uniformF  uStarted =<< use wldStarted


  -- Draw the local player's hands
  handPoses <- use $ wldPlayer . plrHandPoses
  forM_ handPoses $ \handPose -> do

    let finalMatrix = transformationFromPose $ shiftBy handOffset handPose
        rotateVec = rotate ( handPose ^. posOrientation ) (V3 0 0 1)

    uniformV3 uParameterA $ handPose ^. posPosition
    uniformV3 uParameterB $ rotateVec

    drawShape finalMatrix projectionView 0 hand

drawRemoteHands :: (MonadIO m, MonadState World m) 
                => M44 GLfloat -> Shape Uniforms -> m ()
drawRemoteHands projectionView hand = do
  let Uniforms{..} = sUniforms hand
   
  uniformF  uStarted =<< use wldStarted
  uniformF  uDayNight =<< dayNightCycleAt <$> use wldTime
  uniformF  uDayLength dayLength


  players <- use $ wldPlayers . to Map.toList
  forM_ players $ \(_playerID, player) -> 
    forM_ (player ^. plrHandPoses) $ \handPose -> do

      let finalMatrix = transformationFromPose $ shiftBy handOffset handPose
          rotateVec = rotate ( handPose ^. posOrientation ) (V3 0 0 1) 
      
      uniformV3 uParameterA $ handPose ^. posPosition
      uniformV3 uParameterB rotateVec
      
      drawShape finalMatrix projectionView 0 hand

drawRemoteHeads :: (MonadIO m, MonadState World m) 
                => M44 GLfloat
                -> V3 GLfloat
                -> Shape Uniforms
                -> [V3 GLfloat]
                -> m ()
drawRemoteHeads projectionView eyePos face lights = do
  let Uniforms{..} = sUniforms face
  -- Draw all remote players' heads 
  -- (we don't draw the local player's head)
  useProgram (sProgram face)
  uniformV3 uCamera eyePos
  uniformF  uTime =<< use wldTime
  uniformF  uDayNight =<< dayNightCycleAt <$> use wldTime
  uniformF  uDayLength dayLength
  
  setLightUniforms face lights

  withVAO (sVAO face) $ do
    players <- use $ wldPlayers . to Map.toList
    forM_ players $ \(_playerID, player) -> do
      let finalMatrix = transformationFromPose (totalHeadPose player)

      drawShape finalMatrix projectionView 0 face

drawRoom :: (MonadIO m, MonadState World m) 
         => Shape Uniforms
         -> M44 GLfloat
         -> V3 GLfloat
         -> [V3 GLfloat]
         -> GLfloat
         -> m ()
drawRoom plane projectionView eyePos lights filledness = do
  let Uniforms{..} = sUniforms plane

  


  useProgram (sProgram plane)

  uniformF  uTime =<< use wldTime
  uniformF  uStarted =<< use wldStarted
  uniformF  uDayNight =<< dayNightCycleAt <$> use wldTime
  uniformF  uDayLength dayLength

  player <- use wldPlayer 

  let Pose totalHeadPosition totalHeadOriention = totalHeadPose player
  
  let rotateVec = rotate totalHeadOriention (V3 0 0 1)   

  uniformV3 uParameterA totalHeadPosition
  uniformV3 uParameterB rotateVec

  uniformF uFilledness filledness
  -- uniformF uComplete   =<< use wldComplete

  kickVoiceID <- use wldKickVoiceID
  tick <- fromMaybe 0 <$> use (wldVoicePitch . at kickVoiceID)
  uniformF uTick tick

  -- printIO view
  uniformV3 uCamera eyePos

  setLightUniforms plane lights
  
  withVAO (sVAO plane) $ do

    glEnable GL_CULL_FACE
    glCullFace GL_FRONT

    let model = mkTransformation 
            ( axisAngle ( V3 1 0 0 ) 0 )
            ( V3 0 0 0 )

    drawShape model projectionView 0 plane

drawShape :: MonadIO m => M44 GLfloat -> M44 GLfloat -> GLfloat -> Shape Uniforms -> m ()
drawShape model projectionView drawID shape = do 

  let Uniforms{..} = sUniforms shape

  uniformM44 uModelViewProjection (projectionView !*! model)
  uniformM44 uInverseModel        (fromMaybe model (inv44 model))
  uniformM44 uModel model

  uniformF uID drawID

  let vc = vertCount (sGeometry shape) 
  glDrawElements GL_TRIANGLES vc GL_UNSIGNED_INT nullPtr


