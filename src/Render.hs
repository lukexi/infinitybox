{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts, LambdaCase #-}

module Render where

import Graphics.GL

import Linear

import Control.Monad
import Control.Monad.State.Strict
import Control.Lens hiding (view)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Time

import Graphics.GL.Pal2

import Game.Pal
import Types
import Resources




render :: (MonadIO m, MonadState World m) 
       => Resources
       -> M44 GLfloat
       -> M44 GLfloat
       -> m ()
render Resources{..} projection view = do

  time <- realToFrac . utctDayTime <$> liftIO getCurrentTime

  newCubes  <- use wldCubes
  lastCubes <- use wldLastCubes

  let projectionView = projection !*! view
      eyePos = fromMaybe view (inv44 view) ^. translation

  wldEyeDebug .= eyePos

  -- FIXME(lukexi) fix this horrible abomination
  localHandPoses <- use $ wldPlayer . plrHandPoses
  let (light1, light2)  = case map (shiftBy (V3 0 0 (-0.5))) localHandPoses of
        [left,right] -> (left, right)
        _            -> (0, 0)
     


  localPlayerID <- use wldPlayerID
  remoteHandPoses <- use $ wldPlayers . to Map.toList . to (filter (\(playerID, _) -> playerID /= localPlayerID))
  let (light3, light4) = case remoteHandPoses of
        ((_,x):_) -> case map (^. posPosition) (x ^. plrHandPoses) of
          [left,right] -> (left, right)
          _            -> (0, 0)
        _ -> (0,0)

  -- let light1 = V3 9 0 0 
  -- let light2 = V3 (-5) 0 0 
  -- let light3 = V3 0 5 0 
  -- let light4 = V3 0 (-5) 0 

  ------------
  -- LIGHTS --
  ------------
  drawLights light projectionView light1 light2 light3 light4


  -----------
  -- CUBES --
  -----------
  useProgram (program cube)

  -- putStrLnIO (show view)
  let cam = uCamera (uniforms cube)
  uniformV3 cam eyePos
  uniformF (uTime (uniforms cube)) time

  setLightUniforms cube light1 light2 light3 light4


  withVAO (vAO cube) $ do

    glDisable GL_CULL_FACE
    glCullFace GL_BACK

    let cubes = Map.unionWith interpolateObjects lastCubes newCubes
    forM_ ( zip [0..] ( Map.toList cubes ) ) $ \( i , (_objID, obj) ) -> do


      uniformF ( uParameter1 (uniforms cube)) ( obj ^. objPose . posPosition . _x )
      uniformF ( uParameter2 (uniforms cube)) ( obj ^. objPose . posPosition . _y )
      uniformF ( uParameter3 (uniforms cube)) ( obj ^. objPose . posPosition . _z )
      let rotateVec = rotate (obj ^. objPose . posOrientation) (V3 0 0 1) 
      uniformF ( uParameter6 (uniforms cube)) ( rotateVec ^. _z )
      uniformF ( uParameter4 (uniforms cube)) ( rotateVec ^. _x )
      uniformF ( uParameter5 (uniforms cube)) ( rotateVec ^. _y )

      let model = mkTransformation (obj ^. objPose . posOrientation) (obj ^. objPose . posPosition)

      drawEntity model projectionView i cube

  -------------
  -- PLAYERS --
  -------------

  setLightUniforms cube light1 light2 light3 light4
  useProgram (program hand)
  withVAO (vAO hand) $ do

    glEnable GL_CULL_FACE
    glCullFace GL_BACK

    -- Draw the local player's hands
    --metro1Time <- use wldMetro1
    --metro2Time <- use wldMetro2
    handPoses <- use $ wldPlayer . plrHandPoses
    --forM_ (zip handPoses [metro1Time, metro2Time]) $ \(Pose posit orient, metroTime) -> do
    forM_ handPoses $ \handPose -> do
      let shiftPos = shiftBy ( V3 0 0 (-0.25) ) handPose
          finalMatrix = mkTransformation  ( handPose ^. posOrientation ) shiftPos

          rotateVec = rotate ( handPose ^. posOrientation ) (V3 0 0 1)

      uniformF ( uParameter1 (uniforms hand)) ( handPose ^. posPosition . _x )
      uniformF ( uParameter2 (uniforms hand)) ( handPose ^. posPosition . _y )
      uniformF ( uParameter3 (uniforms hand)) ( handPose ^. posPosition . _z )

      uniformF ( uParameter6 (uniforms hand)) ( rotateVec ^. _z )
      uniformF ( uParameter4 (uniforms hand)) ( rotateVec ^. _x )
      uniformF ( uParameter5 (uniforms hand)) ( rotateVec ^. _y )

      drawEntity finalMatrix projectionView 0 hand

    -- Draw all remote players' hands
    players <- use $ wldPlayers . to Map.toList
    forM_ players $ \(playerID, player) -> 
      when (playerID /= localPlayerID) $ do
        forM_ (player ^. plrHandPoses) $ \handPose -> do
          let shiftPos = shiftBy ( V3 0 0 (-0.25) ) handPose
              finalMatrix = mkTransformation  ( handPose ^. posOrientation ) shiftPos
              
              rotateVec = rotate ( handPose ^. posOrientation ) (V3 0 0 1) 
          
          uniformF ( uParameter1 (uniforms hand)) ( handPose ^. posPosition . _x )
          uniformF ( uParameter2 (uniforms hand)) ( handPose ^. posPosition . _y )
          uniformF ( uParameter3 (uniforms hand)) ( handPose ^. posPosition . _z )

          uniformF ( uParameter6 (uniforms hand)) ( rotateVec ^. _z )
          uniformF ( uParameter4 (uniforms hand)) ( rotateVec ^. _x )
          uniformF ( uParameter5 (uniforms hand)) ( rotateVec ^. _y )
        
          drawEntity finalMatrix projectionView 0 hand
  -- Draw all remote players' heads 
  -- (we don't draw the local player's head)
  withVAO (vAO face) $ do
    players <- use $ wldPlayers . to Map.toList
    forM_ players $ \(playerID, player) -> 
      when (playerID /= localPlayerID) $ do
        let shiftPos = shiftBy ( V3 0 0 5 ) ( player ^. plrPose )
            finalMatrix = mkTransformation  ( player ^. plrPose . posOrientation ) shiftPos
        
        drawEntity finalMatrix projectionView 0 cube

  

  --------------------
  -- BACKGROUND BOX --
  --------------------
  useProgram (program plane)

  player <- use wldPlayer 

  Pose totalHeadPosition totalHeadOriention <- totalHeadPose
  
  let rotateVec = rotate totalHeadOriention (V3 0 0 1)   

  uniformF ( uParameter1 (uniforms plane)) ( totalHeadPosition ^. _x )
  uniformF ( uParameter2 (uniforms plane)) ( totalHeadPosition ^. _y )
  uniformF ( uParameter3 (uniforms plane)) ( totalHeadPosition ^. _z )

  uniformF ( uParameter6 (uniforms plane)) ( rotateVec ^. _z )
  uniformF ( uParameter4 (uniforms plane)) ( rotateVec ^. _x )
  uniformF ( uParameter5 (uniforms plane)) ( rotateVec ^. _y )


  --uniformF ( uParameter1 (uniforms plane)) ( sin $ time * 0.3)
  --uniformF ( uParameter2 (uniforms plane)) ( sin $ time * 0.01)
  --uniformF ( uParameter3 (uniforms plane)) ( sin $ time * 0.23)

  --uniformF ( uParameter6 (uniforms plane)) ( sin $ time * 0.074)
  --uniformF ( uParameter4 (uniforms plane)) ( sin $ time * 0.037)
  --uniformF ( uParameter5 (uniforms plane)) ( sin $ time * 0.69 )




  -- printIO view
  let planeCamU = uCamera ( uniforms plane )
  uniformV3 planeCamU eyePos

  setLightUniforms plane light1 light2 light3 light4
   

    
  withVAO (vAO plane) $ do

    glEnable GL_CULL_FACE
    glCullFace GL_FRONT

    let model = mkTransformation 
            ( axisAngle ( V3 1 0 0 ) 0 )
            ( V3 0 0 0 )

    drawEntity model projectionView 0 plane


setLightUniforms :: (MonadIO m) 
                 => Entity Uniforms
                 -> V3 GLfloat -> V3 GLfloat -> V3 GLfloat -> V3 GLfloat -> m ()
setLightUniforms anEntity l1 l2 l3 l4 = do

  let light1 = uLight1 (uniforms anEntity)
  uniformV3 light1 l1

  let light2 = uLight2 (uniforms anEntity)
  uniformV3 light2 l2

  let light3 = uLight3 (uniforms anEntity)
  uniformV3 light3 l3

  let light4 = uLight4 (uniforms anEntity)
  uniformV3 light4 l4


drawLights :: MonadIO m => Entity Uniforms
           -> M44 GLfloat
           -> V3 GLfloat
           -> V3 GLfloat
           -> V3 GLfloat
           -> V3 GLfloat
           -> m ()
drawLights anEntity projectionView l1 l2 l3 l4 = do

  useProgram (program anEntity)

  withVAO (vAO anEntity) $ do

    glEnable GL_CULL_FACE
    glCullFace GL_BACK

    forM_ (zip [0..] [l1, l2, l3, l4]) $ \(i, lightPos) -> do
      let model = mkTransformation (axisAngle (V3 1 0 0) 0.0) lightPos
      uniformF ( uParameter1 (uniforms anEntity)) ( lightPos ^. _x )
      uniformF ( uParameter2 (uniforms anEntity)) ( lightPos ^. _y )
      uniformF ( uParameter3 (uniforms anEntity)) ( lightPos ^. _z )
      drawEntity model projectionView i anEntity



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


shiftBy :: V3 GLfloat -> Pose -> V3 GLfloat
shiftBy vec pose  = (pose ^. posPosition) + rotate (pose ^. posOrientation) vec
