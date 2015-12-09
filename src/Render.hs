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
import Data.Map.Strict (Map)
import Data.Maybe

import Graphics.GL.Pal

import Types
import Data.Monoid


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


render :: (MonadIO m, MonadReader World m) 
       => Shapes
       -> [Player]
       -> Map ObjectID Object
       -> [V3 GLfloat]
       -> M44 GLfloat
       -> M44 GLfloat
       -> m ()
render Shapes{..} players cubes _lights projection viewMat = do

  let projectionView = projection !*! viewMat
      eyePos = inv44 viewMat ^. translation

  --drawLights  light      projectionView        lights
  drawPlayers hand handle face  projectionView eyePos players

  phase <- view wldPhase
  case phase of 
    PhaseVoid ->
      drawLogo logo projectionView eyePos
    PhaseLogo ->
      drawLogo logo projectionView eyePos
    PhaseMain ->
      drawCubes cube projectionView eyePos cubes 
    PhaseEnd  ->
      drawCubes logo  projectionView eyePos cubes

  drawRoom room projectionView eyePos


drawCubes :: (MonadIO m, MonadReader World m) 
          => Shape Uniforms
          -> M44 GLfloat -> V3 GLfloat -> Map ObjectID Object -> m ()
drawCubes cube projectionView eyePos cubes = do
  

  let Uniforms{..} = sUniforms cube
  useProgram (sProgram cube)
  
  uniformV3 uCamera eyePos

  withVAO (sVAO cube) $ do

    glDisable GL_CULL_FACE
    glCullFace GL_BACK
    
    forM_ (Map.toList cubes) $ \(objID, obj) -> do

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

      let model = transformationFromPose (obj ^. objPose)
          -- The server will continuously update us on the scale of the cube
          scaledModel = model !*! scaleMatrix (realToFrac (obj ^. objScale))
      drawShape' scaledModel projectionView cube


drawLogo :: (MonadIO m, MonadReader World m)
          => Shape Uniforms
          -> M44 GLfloat
          -> V3 GLfloat
          -> m ()
drawLogo cube projectionView eyePos = do


  let Uniforms{..} = sUniforms cube
  useProgram (sProgram cube)

  uniformV3 uCamera eyePos

  withVAO (sVAO cube) $ do

    glDisable GL_CULL_FACE
    glCullFace GL_BACK
    
    let obj = logoObject
        model = transformationFromPose (obj ^. objPose)
        scaledModel = model !*! scaleMatrix (realToFrac (obj ^. objScale))

    drawShape' scaledModel projectionView cube


drawLights :: (MonadIO m, MonadReader World m) 
           => Shape Uniforms
           -> M44 GLfloat
           -> [V3 GLfloat]
           -> m ()
drawLights anShape projectionView lights = do
  let Uniforms{..} = sUniforms anShape
  useProgram (sProgram anShape)

  withVAO (sVAO anShape) $ do

    glEnable GL_CULL_FACE
    glCullFace GL_BACK

    forM_ lights $ \lightPos -> do
      let model = mkTransformation (axisAngle (V3 1 0 0) 0.0) lightPos
      
      uniformV3 uParameterA lightPos

      drawShape' model projectionView anShape


drawPlayers :: (MonadIO m, MonadReader World m) 
            => Shape Uniforms
            -> Shape Uniforms
            -> Shape Uniforms
            -> M44 GLfloat
            -> V3 GLfloat
            -> [Player]
            -> m ()
drawPlayers hand handle face projectionView eyePos players = do
  let haveRemotePlayers = not (null players)
  useProgram (sProgram hand)
  let Uniforms{..} = sUniforms hand

  uniformV3 uCamera eyePos

  withVAO (sVAO hand) $ do

    glEnable GL_CULL_FACE
    glCullFace GL_BACK

    drawLocalHands  projectionView hand
    when haveRemotePlayers $ 
      drawRemoteHands projectionView hand players

  drawHandles projectionView eyePos handle players

  when haveRemotePlayers $ 
    drawRemoteHeads projectionView eyePos face players
  -- DEBUG (display local head shifted in space so it can be observed)
  -- localPlayer <- view wldPlayer
  -- let localPlayer' = localPlayer & plrPose %~ shiftBy (V3 (-1) 0 0)
  -- drawRemoteHeads projectionView eyePos face (localPlayer':players)
  -- /DEBUG

drawHandles :: (MonadIO m, MonadReader World m) 
            => M44 GLfloat -> V3 GLfloat -> Shape Uniforms -> [Player] -> m ()
drawHandles projectionView eyePos handle players = do
  let haveRemotePlayers = not (null players)
  useProgram (sProgram handle)
  let Uniforms{..} = sUniforms handle

  uniformV3 uCamera eyePos

  withVAO (sVAO handle) $ do

    glEnable GL_CULL_FACE
    glCullFace GL_BACK

    drawLocalHandles  projectionView handle
    when haveRemotePlayers $ 
      drawRemoteHandles projectionView handle players

drawLocalHands :: (MonadIO m, MonadReader World m) 
               => M44 GLfloat -> Shape Uniforms -> m ()
drawLocalHands projectionView hand = do

  let Uniforms{..} = sUniforms hand

  -- Draw the local player's hands
  handPoses <- view $ wldPlayer . plrHandPoses
  forM_ handPoses $ \handPose -> do

    let finalMatrix = transformationFromPose $ shiftBy handOffset handPose
        rotateVec = rotate (handPose ^. posOrientation) (V3 0 0 1)

    uniformV3 uParameterA $ handPose ^. posPosition
    uniformV3 uParameterB $ rotateVec

    drawShape' finalMatrix projectionView hand


drawRemoteHands :: (MonadIO m, MonadReader World m) 
                => M44 GLfloat -> Shape Uniforms -> [Player] -> m ()
drawRemoteHands projectionView hand players = do
  let Uniforms{..} = sUniforms hand
  
  forM_ players $ \player -> 
    forM_ (player ^. plrHandPoses) $ \handPose -> do

      let finalMatrix = transformationFromPose $ shiftBy handOffset handPose
          rotateVec = rotate (handPose ^. posOrientation) (V3 0 0 1) 
      
      uniformV3 uParameterA $ handPose ^. posPosition
      uniformV3 uParameterB rotateVec
      
      drawShape' finalMatrix projectionView hand


-- drawing handles is the same as drawing the hands
-- but with no offeset, since they will be right on the actual controllers
drawLocalHandles :: (MonadIO m, MonadReader World m) 
               => M44 GLfloat -> Shape Uniforms -> m ()
drawLocalHandles projectionView hand = do

  let Uniforms{..} = sUniforms hand

  -- Draw the local player's hands
  handPoses <- view $ wldPlayer . plrHandPoses
  forM_ handPoses $ \handPose -> do

    let finalMatrix = transformationFromPose (shiftBy handleOffset handPose)
        rotateVec   = rotate (handPose ^. posOrientation) (V3 0 0 1)

    uniformV3 uParameterA $ handPose ^. posPosition
    uniformV3 uParameterB $ rotateVec

    drawShape' finalMatrix projectionView hand

drawRemoteHandles :: (MonadIO m, MonadReader World m) 
                => M44 GLfloat -> Shape Uniforms -> [Player] -> m ()
drawRemoteHandles projectionView hand players = do
  let Uniforms{..} = sUniforms hand
   
  forM_ players $ \player -> 
    forM_ (player ^. plrHandPoses) $ \handPose -> do

      let finalMatrix = transformationFromPose (shiftBy handleOffset handPose)
          rotateVec   = rotate (handPose ^. posOrientation) (V3 0 0 1) 
      
      uniformV3 uParameterA (handPose ^. posPosition)
      uniformV3 uParameterB rotateVec
      
      drawShape' finalMatrix projectionView hand


drawRemoteHeads :: (MonadIO m, MonadReader World m) 
                => M44 GLfloat
                -> V3 GLfloat
                -> Shape Uniforms
                -> [Player]
                -> m ()
drawRemoteHeads projectionView eyePos face players = do
  let Uniforms{..} = sUniforms face
  -- Draw all remote players' heads 
  -- (we don't draw the local player's head)
  useProgram (sProgram face)
  uniformV3 uCamera eyePos
  
  withVAO (sVAO face) $ do
    forM_ players $ \player -> do
      let finalMatrix = transformationFromPose (totalHeadPose player)

      drawShape' finalMatrix projectionView face


drawRoom :: MonadIO m => Shape Uniforms -> M44 GLfloat -> V3 GLfloat -> m ()
drawRoom room projectionView eyePos = do
  let Uniforms{..} = sUniforms room

  useProgram (sProgram room)

  uniformV3 uCamera eyePos
  
  withVAO (sVAO room) $ do

    glEnable GL_CULL_FACE
    glCullFace GL_FRONT

    let model = identity

    drawShape' model projectionView room

drawShape' :: MonadIO m => M44 GLfloat -> M44 GLfloat -> Shape Uniforms -> m ()
drawShape' model projectionView shape = do 

  let Uniforms{..} = sUniforms shape

  uniformM44 uModelViewProjection (projectionView !*! model)
  uniformM44 uInverseModel        (inv44 model)
  uniformM44 uModel model

  let vc = geoVertCount (sGeometry shape) 
  glDrawElements GL_TRIANGLES vc GL_UNSIGNED_INT nullPtr


