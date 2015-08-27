{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts, LambdaCase #-}

module Render where

import Graphics.GL

import Linear

import Control.Monad
import Control.Monad.State.Strict
import Control.Lens
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Time

import Graphics.GL.Pal2

import Game.Pal
import Types
import Resources

listToTuple _   [a,b] = (a,b)
listToTuple def _     = def

useListOf aLens = do
  stat <- use id
  return (stat ^.. aLens)

getLocalHandPositions :: MonadState World m => m (V3 GLfloat, V3 GLfloat)
getLocalHandPositions = listToTuple (0,0) <$>
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
getFirstRemoteHandPositions :: MonadState World m => m (V3 GLfloat, V3 GLfloat)
getFirstRemoteHandPositions = do
  remoteHandPoses <- listToMaybe <$> useListOf (wldPlayers . traverse . plrHandPoses)
  return $ 
    case remoteHandPoses of
      Just hands -> listToTuple (0,0) (hands ^.. traverse . posPosition)
      Nothing -> (0,0)

render :: (MonadIO m, MonadState World m) 
       => Resources
       -> M44 GLfloat
       -> M44 GLfloat
       -> m ()
render Resources{..} projection viewMat = do

  let projectionView = projection !*! viewMat
      eyePos = fromMaybe viewMat (inv44 viewMat) ^. translation

  (light1, light2) <- getLocalHandPositions
  (light3, light4) <- getFirstRemoteHandPositions
  let lightPositions = (light1, light2, light3, light4)

  drawLights light      projectionView lightPositions

  drawCubes   cube      projectionView eyePos lightPositions

  drawPlayers hand face projectionView eyePos lightPositions

  drawRoom    plane     projectionView eyePos lightPositions
  
drawCubes :: (MonadIO m, MonadState World m)
          => Entity Uniforms
          -> M44 GLfloat
          -> V3 GLfloat
          -> (V3 GLfloat, V3 GLfloat, V3 GLfloat, V3 GLfloat)
          -> m ()
drawCubes cube projectionView eyePos lights  = do
  -- Interpolate between the last and newest cube states
  newCubes  <- use wldCubes
  lastCubes <- use wldLastCubes
  let cubes = Map.unionWith interpolateObjects lastCubes newCubes


  time <- realToFrac . utctDayTime <$> liftIO getCurrentTime

  let Uniforms{..} = uniforms cube
  useProgram (program cube)

  -- putStrLnIO (show view)
  
  uniformV3 uCamera eyePos
  
  uniformF  uTime time

  setLightUniforms cube lights

  withVAO (vAO cube) $ do

    glDisable GL_CULL_FACE
    glCullFace GL_BACK
    
    forM_ ( zip [0..] ( Map.toList cubes ) ) $ \( i , (objID, obj) ) -> do

      tick <- fromMaybe 0 <$> use (wldPatchOutput . at objID)
      uniformF uTick tick

      let rotateVec = rotate (obj ^. objPose . posOrientation) (V3 0 0 1) 
      
      uniformF uParameter1 $ obj ^. objPose . posPosition . _x
      uniformF uParameter2 $ obj ^. objPose . posPosition . _y
      uniformF uParameter3 $ obj ^. objPose . posPosition . _z
      
      uniformF uParameter6 $ rotateVec ^. _z
      uniformF uParameter4 $ rotateVec ^. _x
      uniformF uParameter5 $ rotateVec ^. _y

      let model = transformationFromPose (obj ^. objPose)

      drawEntity model projectionView i cube

setLightUniforms :: (MonadIO m) 
                 => Entity Uniforms
                 -> (V3 GLfloat, V3 GLfloat, V3 GLfloat, V3 GLfloat) 
                 -> m ()
setLightUniforms anEntity (l1, l2, l3, l4) = do
  let Uniforms{..} = uniforms anEntity
  
  uniformV3 uLight1 l1
  uniformV3 uLight2 l2
  uniformV3 uLight3 l3
  uniformV3 uLight4 l4


drawLights :: MonadIO m 
           => Entity Uniforms
           -> M44 GLfloat
           -> (V3 GLfloat, V3 GLfloat, V3 GLfloat, V3 GLfloat) 
           -> m ()
drawLights anEntity projectionView (l1, l2, l3, l4) = do
  let Uniforms{..} = uniforms anEntity
  useProgram (program anEntity)

  withVAO (vAO anEntity) $ do

    glEnable GL_CULL_FACE
    glCullFace GL_BACK

    forM_ (zip [0..] [l1, l2, l3, l4]) $ \(i, lightPos) -> do
      let model = mkTransformation (axisAngle (V3 1 0 0) 0.0) lightPos
      
      uniformF uParameter1 $ lightPos ^. _x
      uniformF uParameter2 $ lightPos ^. _y
      uniformF uParameter3 $ lightPos ^. _z
      
      drawEntity model projectionView i anEntity

drawPlayers :: (MonadIO m, MonadState World m) 
            => Entity Uniforms
            -> Entity Uniforms
            -> M44 GLfloat
            -> V3 GLfloat
            -> (V3 GLfloat, V3 GLfloat, V3 GLfloat, V3 GLfloat)
            -> m ()
drawPlayers hand face projectionView eyePos lights = do
  useProgram (program hand)
  let Uniforms{..} = uniforms hand
  uniformV3 uCamera eyePos
  setLightUniforms hand lights

  withVAO (vAO hand) $ do

    glEnable GL_CULL_FACE
    glCullFace GL_BACK

    drawLocalHands projectionView hand
    drawRemoteHands projectionView hand

  drawRemoteHeads projectionView eyePos face lights

drawLocalHands :: (MonadIO m, MonadState World m) 
               => M44 GLfloat -> Entity Uniforms -> m ()
drawLocalHands projectionView hand = do
  let Uniforms{..} = uniforms hand
  -- Draw the local player's hands
  handPoses <- use $ wldPlayer . plrHandPoses
  forM_ handPoses $ \handPose -> do
    let finalMatrix = transformationFromPose $ shiftBy handOffset handPose
        rotateVec = rotate ( handPose ^. posOrientation ) (V3 0 0 1)

    uniformF uParameter1 $ handPose ^. posPosition . _x
    uniformF uParameter2 $ handPose ^. posPosition . _y
    uniformF uParameter3 $ handPose ^. posPosition . _z
    
    uniformF uParameter6 $ rotateVec ^. _z
    uniformF uParameter4 $ rotateVec ^. _x
    uniformF uParameter5 $ rotateVec ^. _y

    drawEntity finalMatrix projectionView 0 hand

drawRemoteHands :: (MonadIO m, MonadState World m) 
                => M44 GLfloat -> Entity Uniforms -> m ()
drawRemoteHands projectionView hand = do
  let Uniforms{..} = uniforms hand
  players <- use $ wldPlayers . to Map.toList
  forM_ players $ \(playerID, player) -> 
    forM_ (player ^. plrHandPoses) $ \handPose -> do
      let finalMatrix = transformationFromPose $ shiftBy handOffset handPose
          
          rotateVec = rotate ( handPose ^. posOrientation ) (V3 0 0 1) 
      
      uniformF uParameter1 $ handPose ^. posPosition . _x
      uniformF uParameter2 $ handPose ^. posPosition . _y
      uniformF uParameter3 $ handPose ^. posPosition . _z
      
      uniformF uParameter6 $ rotateVec ^. _z
      uniformF uParameter4 $ rotateVec ^. _x
      uniformF uParameter5 $ rotateVec ^. _y
      
      drawEntity finalMatrix projectionView 0 hand

drawRemoteHeads :: (MonadIO m, MonadState World m) 
                => M44 GLfloat
                -> V3 GLfloat
                -> Entity Uniforms
                -> (V3 GLfloat, V3 GLfloat, V3 GLfloat, V3 GLfloat)
                -> m ()
drawRemoteHeads projectionView eyePos face lights = do
  let Uniforms{..} = uniforms face
  -- Draw all remote players' heads 
  -- (we don't draw the local player's head)
  useProgram (program face)
  uniformV3 uCamera eyePos
  setLightUniforms face lights

  withVAO (vAO face) $ do
    players <- use $ wldPlayers . to Map.toList
    forM_ players $ \(playerID, player) -> do
      let finalMatrix = transformationFromPose (totalHeadPose player)

      drawEntity finalMatrix projectionView 0 face

drawRoom :: (MonadIO m, MonadState World m) 
         => Entity Uniforms
         -> M44 GLfloat
         -> V3 GLfloat
         -> (V3 GLfloat, V3 GLfloat, V3 GLfloat, V3 GLfloat)
         -> m ()
drawRoom plane projectionView eyePos lights = do
  let Uniforms{..} = uniforms plane

  useProgram (program plane)

  player <- use wldPlayer 

  let Pose totalHeadPosition totalHeadOriention = totalHeadPose player
  
  let rotateVec = rotate totalHeadOriention (V3 0 0 1)   

  uniformF uParameter1 $ totalHeadPosition ^. _x
  uniformF uParameter2 $ totalHeadPosition ^. _y
  uniformF uParameter3 $ totalHeadPosition ^. _z
  
  uniformF uParameter4 $ rotateVec ^. _x
  uniformF uParameter5 $ rotateVec ^. _y
  uniformF uParameter6 $ rotateVec ^. _z


  --uniformF uParameter1 ( sin $ time * 0.3)
  --uniformF uParameter2 ( sin $ time * 0.01)
  --uniformF uParameter3 ( sin $ time * 0.23)
  --uniformF uParameter6 ( sin $ time * 0.074)
  --uniformF uParameter4 ( sin $ time * 0.037)
  --uniformF uParameter5 ( sin $ time * 0.69 )

  -- printIO view
  uniformV3 uCamera eyePos

  setLightUniforms plane lights
  
  withVAO (vAO plane) $ do

    glEnable GL_CULL_FACE
    glCullFace GL_FRONT

    let model = mkTransformation 
            ( axisAngle ( V3 1 0 0 ) 0 )
            ( V3 0 0 0 )

    drawEntity model projectionView 0 plane

drawEntity :: MonadIO m => M44 GLfloat -> M44 GLfloat -> GLfloat -> Entity Uniforms -> m ()
drawEntity model projectionView drawID anEntity = do 

  let Uniforms{..} = uniforms anEntity

  uniformM44 uModelViewProjection (projectionView !*! model)
  uniformM44 uInverseModel        (fromMaybe model (inv44 model))
  uniformM44 uModel model

  let dID = uID 
  glUniform1f (unUniformLocation dID) drawID

  let vc = vertCount (geometry anEntity) 
  glDrawElements GL_TRIANGLES vc GL_UNSIGNED_INT nullPtr


