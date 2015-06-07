{-# LANGUAGE FlexibleContexts, LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
import Graphics.UI.GLFW.Pal

-- import Halive.Utils
import Graphics.GL
import Graphics.Oculus
import Linear


import Control.Monad
import Control.Monad.State
import System.Random
import Control.Lens

import Control.Monad.Random

import Pal.Pal
import Pal.Geometries.CubeInfo
import Pal.Geometries.PlaneInfo


import Network.Sox
import Network.ReceiveChan

import Types
import Randoms
import Movement
import Control.Monad.Free.Binary ()
import Control.Monad.Free.FromFreeT
import qualified Data.Map as Map
import Data.Maybe

enableVR :: Bool
-- enableVR = False
enableVR = True

main :: IO ()
main = asClient $ \s -> do

  -- Initialize GLFW

  (resX, resY, maybeHMD) <- if enableVR 
    then do
      hmd <- createHMD
      (resX, resY) <- getHMDResolution hmd
      return (resX, resY, Just hmd)
    else return (1024, 768, Nothing)
  
  (window, events) <- createWindow "UDPCubes" resX resY
  -- Compensate for retina framebuffers on Mac
  (frameW, frameH) <- getFramebufferSize window
  when (frameW > resX && frameH > resY) $
    setWindowSize window (resX `div` 2) (resY `div` 2)

  -- Set up Oculus
  maybeRenderHMD <- forM maybeHMD $ \hmd -> do
    renderHMD <- configureHMDRendering hmd "UDPCubes"
    dismissHSWDisplay hmd
    recenterPose hmd
    return renderHMD

  -- Get a stdgen for Entity ID generation
  stdGen <- getStdGen


  -- Create a UDP receive thread
  receiveChan <- makeReceiveChan s

  -- Connect to the server
  _bytesSent <- sendInstrs s (compile stdGen (connect "player"))


  -- Set up our cube resources
  cubeProg <- createShaderProgram "src/shaders/cube.vert" "src/shaders/cube.frag"
  cubeGeometry <- initCubeGeometry 0.2

  cube <- entity cubeGeometry cubeProg 

  -- Set up our cube resources
  planeProg <- createShaderProgram "src/shaders/cube.vert" "src/shaders/cube.frag"
  planeGeometry <- initPlaneGeometry 50

  plane <- entity planeGeometry planeProg 

  -- Set up GL state
  glEnable GL_DEPTH_TEST
  glClearColor 0 0 0.1 1

  -- Begin game loop
  void . flip runRandT stdGen . flip runStateT newWorld . whileWindow window $ do
    

    -- Update interpolation buffer
    wldLastCubes <~ use wldCubes

    -- Handle network events
    readChanAll receiveChan interpret

    -- Handle key events
    processEvents events $ \e -> do
      closeOnEscape window e
      keyDown Key'E e (addCube s)
      keyDown Key'F e (setCursorInputMode window CursorInputMode'Disabled)
      keyDown Key'G e (setCursorInputMode window CursorInputMode'Normal)

    -- Handle mouse events
    isFocused <- getWindowFocused window
    when isFocused $ applyMouseLook window

    -- Handle movement events
    applyMovement window

    -- Render the scene
    case maybeRenderHMD of
      Nothing -> renderFlat window cube plane
      Just renderHMD -> renderVR renderHMD cube plane

renderVR :: (MonadIO m, MonadState World m) 
         => RenderHMD -> Entity -> Entity -> m ()
renderVR renderHMD cube plane = do
  renderHMDFrame renderHMD $ \eyePoses -> do

    glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)

    viewMat <- playerViewMat

    renderHMDEyes renderHMD eyePoses $ \projMat -> do
      render cube plane (projMat !*! viewMat)

renderFlat :: (MonadIO m, MonadState World m) 
           => Window -> Entity -> Entity -> m ()
renderFlat win cube plane = do

  glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
  
  projMat <- makeProjection win
  viewMat <- playerViewMat

  let viewProj = projMat !*! viewMat
  render cube plane viewProj
  swapBuffers win





render :: ( MonadIO m, MonadState World m ) => Entity -> Entity -> V4 (V4 GLfloat) -> m ()
render cube plane viewProj = do

  newCubes  <- use wldCubes
  lastCubes <- use wldLastCubes

  cameraPos <- use (wldPlayer . plrPosition)

  -- Begin cube batch
  useProgram (program cube)

  let cam = uCamera ( uniforms cube )
  glUniform3f ( unUniformLocation  cam )
      ( cameraPos ^. _x )
      ( cameraPos ^. _y )
      ( cameraPos ^. _z )

  withVAO ( vAO cube ) $ do

    -- let cubes = lerp 0.5 newCubes lastCubes
    let cubes = Map.unionWith interpolateObjects lastCubes newCubes
    forM_ cubes $ \obj -> do

      let model = mkTransformation (obj ^. objOrientation) (obj ^. objPosition)

      drawEntity model viewProj cube


  withVAO (vAO plane) $ do

    let model = mkTransformation 
            ( axisAngle ( V3 1 0 0 ) ((-pi)/2) )
            ( V3 0 0 0 )

    drawEntity model viewProj plane



drawEntity model projection entity = do 

  let Uniforms{..} = uniforms entity

  uniformM44 uMVP ( projection !*! model)
  uniformM44 uInverseModel (fromMaybe model (inv44 model))
  uniformM44 uModel model

  glDrawArrays GL_TRIANGLES 0 ( vertCount ( geometry entity ) )




addCube :: (MonadIO m, MonadState World m, MonadRandom m) => Socket -> m ()
addCube s = do
  -- Spawn a cube at the player's position and orientation
  instructions <- fromFreeT $ do
    playerPos <- use (wldPlayer . plrPosition)
    playerRot <- use (wldPlayer . plrOrientation)
    let spawnPoint = rotate playerRot (V3 0 0.1 0) + playerPos
        object = Object spawnPoint playerRot 0.2
    
    objID <- getRandom'
    update objID object

  -- instructions <- randomCube
  
  interpret instructions

  _bytesSent <- sendInstrs s instructions
  return ()

  
  
