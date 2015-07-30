{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts, LambdaCase #-}

module Render where

import Graphics.UI.GLFW.Pal

import Graphics.GL
import Graphics.Oculus
import Linear

import Control.Monad
import Control.Monad.State
import Control.Lens hiding (view)
import qualified Data.Map as Map
import Data.Maybe

import Graphics.GL.Pal2

import Game.Pal
import Types
import Resources


renderVR :: (MonadIO m, MonadState World m) 
         => RenderHMD -> Resources -> m ()
renderVR renderHMD resources = renderHMDFrame renderHMD $ \eyePoses -> do

  glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)

  view <- viewMatrixFromPose <$> use (wldPlayer . plrPose)

  renderHMDEyes renderHMD eyePoses $ \projection eyeView -> do

    let finalView = eyeView !*! view 

    render resources projection finalView 

renderFlat :: (MonadIO m, MonadState World m) 
           => Window -> Resources -> m ()
renderFlat win resources = do

  glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
  
  projection  <- makeProjection win
  view <- viewMatrixFromPose <$> use (wldPlayer . plrPose)

  render resources projection view

  swapBuffers win

render :: (MonadIO m, MonadState World m) 
       => Resources
       -> M44 GLfloat
       -> M44 GLfloat
       -> m ()
render Resources{..} projection view = do

  newCubes  <- use wldCubes
  lastCubes <- use wldLastCubes

    let projectionView = projection !*! view
        eyePos = fromMaybe view (inv44 view) ^. translation

  wldEyeDebug .= eyePos

  -- FIXME(lukexi) fix this horrible abomination
  localHandPoses <- use $ wldPlayer . plrHandPoses
  let (light1, light2)  = case map (^. posPosition) localHandPoses of
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

  setLightUniforms cube light1 light2 light3 light4

  withVAO (vAO cube) $ do

    glEnable GL_CULL_FACE
    glCullFace GL_BACK

    let cubes = Map.unionWith interpolateObjects lastCubes newCubes
    forM_ ( zip [0..] ( Map.elems cubes ) ) $ \( i , obj ) -> do

      let model = mkTransformation (obj ^. objPose . posOrientation) (obj ^. objPose . posPosition)

      drawEntity model projectionView i cube

  -------------
  -- PLAYERS --
  -------------
  withVAO (vAO hand) $ do

    glEnable GL_CULL_FACE
    glCullFace GL_BACK

    -- Draw the local player's hands
    metro1Time <- use wldMetro1
    metro2Time <- use wldMetro2
    handPoses <- use $ wldPlayer . plrHandPoses
    forM_ (zip handPoses [metro1Time, metro2Time]) $ \(Pose posit orient, metroTime) -> do
      let model = mkTransformation orient posit
      uniformF (uBeat (uniforms cube)) metroTime
      drawEntity model projectionView 0 cube 

    -- Draw all remote players' hands
    players <- use $ wldPlayers . to Map.toList
    forM_ players $ \(playerID, player) -> 
      when (playerID /= localPlayerID) $ do
        forM_ (player ^. plrHandPoses) $ \handPose -> do
          drawEntity (poseToMatrix handPose) projectionView 0 cube 

  -- Draw all remote players' heads 
  -- (we don't draw the local player's head)
  withVAO (vAO face) $ do
    players <- use $ wldPlayers . to Map.toList
    forM_ players $ \(playerID, player) -> 
      when (playerID /= localPlayerID) $ do
        drawEntity (poseToMatrix (player ^. plrPose)) projectionView 0 cube

  

  --------------------
  -- BACKGROUND BOX --
  --------------------
  useProgram (program plane)

  -- printIO view
  let planeCamU = uCamera ( uniforms plane )
  uniformV3 planeCamU eyePos

  setLightUniforms plane light1 light2 light3 light4


  withVAO (vAO plane) $ do

    glEnable GL_CULL_FACE
    glCullFace GL_FRONT

    let model = mkTransformation 
            ( axisAngle ( V3 1 0 0 ) 0.0 )
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


