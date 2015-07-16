{-# LANGUAGE FlexibleContexts, LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Client where
import Graphics.UI.GLFW.Pal

-- import Halive.Utils
import Graphics.GL
import Graphics.Oculus
import Linear

import System.Hardware.Hydra

import Control.Monad
import Control.Monad.State
import System.Random
import Control.Lens hiding (view)

import Control.Monad.Random

import Graphics.GL.Pal2

import Network.UDP.Pal

import Types
import Movement
import qualified Data.Map as Map
import Data.Maybe
import Control.Concurrent.STM

enableVR :: Bool
--enableVR = False
enableVR = True

writeTransceiver :: MonadIO m => Transceiver r -> AppPacket r -> m ()
writeTransceiver transceiver = liftIO . atomically . writeTChan (tcOutgoingPackets transceiver)

main :: IO ()
main = do
  -- Set up Hydra
  sixenseBase <- initSixense

  -- Create a UDP receive thread
  transceiver@Transceiver{..} <- createTransceiverToAddress serverName serverPort packetSize

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
  stdGen     <- getStdGen
  
  -- Connect to the server
  -- Initial connection to the server
  playerID <- randomName
  writeTransceiver transceiver $ Reliable (Connect playerID)


  -- Set up our cube resources
  cubeProg   <- createShaderProgram "src/shaders/cube.vert" "src/shaders/cube.frag"
  cubeGeo    <- cubeGeometry ( 0.5 :: V3 GLfloat ) ( V3 1 1 1 )


  -- Set up our cube resources
  lightProg  <- createShaderProgram "src/shaders/light.vert" "src/shaders/light.frag"
  lightGeo   <- cubeGeometry ( V3 0.1 0.1 0.1 ) ( V3 1 1 1 )

  -- Set up our cube resources
  planeProg  <- createShaderProgram "src/shaders/plane.vert" "src/shaders/plane.frag"
  planeGeo   <- cubeGeometry ( V3 20 20 20 ) ( V3 1 1 1 )

  -- Set up our cube resources
  handProg  <- createShaderProgram "src/shaders/plane.vert" "src/shaders/plane.frag"
  handGeo   <- cubeGeometry ( V3 0.2 0.2 1 ) ( V3 1 1 1 )

  -- Set up our cube resources
  faceProg  <- createShaderProgram "src/shaders/plane.vert" "src/shaders/plane.frag"
  faceGeo   <- cubeGeometry ( V3 0.5 0.8 0.1 ) ( V3 1 1 1 )

  plane      <- entity planeGeo planeProg 
  cube       <- entity cubeGeo  cubeProg 
  light      <- entity lightGeo lightProg 
  hand       <- entity handGeo  handProg 
  face       <- entity faceGeo  faceProg 


  -- Set up GL state
  glEnable GL_DEPTH_TEST
  glClearColor 0 0 0.1 1
  --glPolygonMode GL_FRONT_AND_BACK GL_LINE 
  
  -- Begin game loop
  let world = newWorld playerID
  void . flip runRandT stdGen . flip runStateT world . whileWindow window $ do
    frameNumber <- wldFrameNumber <+= 1

    -- Update interpolation buffer
    wldLastCubes <~ use wldCubes

    -- Handle network events
    interpretNetworkPackets tcVerifiedPackets interpret

    -- Get latest Hydra data
    hands <- getHands sixenseBase

    -- Handle mouse events
    isFocused <- getWindowFocused window
    when isFocused $ applyMouseLook window

    -- Handle keyboard movement events
    applyMovement window

    -- Handle Hydra movement events
    case hands of
      [left, right] -> do
        movePlayer (V3 (joystickX left / 10) 0 (-(joystickY left / 10)))
        -- Quat rotation must be rotation * original rather than vice versa
        wldPlayer . plrPose . posOrientation %= \old -> axisAngle (V3 0 1 0) (joystickX right) * old
      _ -> return ()
    
    

    -- Get player pose
    playerPos <- use (wldPlayer . plrPose . posPosition)
    playerRot <- use (wldPlayer . plrPose . posOrientation)

    -- Handle key events
    processEvents events $ \e -> do
      closeOnEscape window e
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
    forM_ (zip hands handWorldPoses) $ \(handData, Pose posit orient) -> do
      when (trigger handData > 0.5 && frameNumber `mod` 30 == 0) $ 
        addCube transceiver posit orient

    -- Send player position
    player <- use wldPlayer
    writeTransceiver transceiver $ Reliable $ UpdatePlayer playerID player

    -- Render the scene
    case maybeRenderHMD of

      Nothing        -> renderFlat window    cube plane light hand face
      Just renderHMD -> renderVR   renderHMD cube plane light hand face





renderVR :: (MonadIO m, MonadState World m) 
         => RenderHMD -> Entity Uniforms -> Entity Uniforms -> Entity Uniforms -> Entity Uniforms -> Entity Uniforms -> m ()
renderVR renderHMD cube plane light hand face = do

  renderHMDFrame renderHMD $ \eyePoses -> do

    glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)

    view <- playerViewMat

    renderHMDEyes renderHMD eyePoses $ \projection eyeView -> do

      let finalView = eyeView !*! view 

      render cube plane light hand face projection finalView 

renderFlat :: (MonadIO m, MonadState World m) 
           => Window -> Entity Uniforms -> Entity Uniforms -> Entity Uniforms -> Entity Uniforms -> Entity Uniforms -> m ()
renderFlat win cube plane light hand face = do

  glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
  
  projection  <- makeProjection win
  view        <- playerViewMat

  render cube plane light hand face projection view

  swapBuffers win

poseToMatrix :: Pose -> M44 GLfloat
poseToMatrix (Pose posit orient) = mkTransformation orient posit

render :: (MonadIO m, MonadState World m) 
       => Entity Uniforms
       -> Entity Uniforms
       -> Entity Uniforms
       -> Entity Uniforms
       -> Entity Uniforms
       -> M44 GLfloat
       -> M44 GLfloat
       -> m ()
render cube plane light hand face projection view = do




  newCubes  <- use wldCubes
  lastCubes <- use wldLastCubes



  let projectionView = projection !*! view

  let eyePos = fromMaybe view (inv44 view) ^. translation

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

  -- putStrLnIO (show view )
  let cam = uCamera ( uniforms cube )
  glUniform3f ( unUniformLocation  cam )
      ( eyePos ^. _x )
      ( eyePos ^. _y )
      ( eyePos ^. _z )

  setLightUniforms cube light1 light2 light3 light4

  withVAO ( vAO cube ) $ do

    glEnable GL_CULL_FACE
    glCullFace GL_BACK


    -- let cubes = lerp 0.5 newCubes lastCubes
    let cubes = Map.unionWith interpolateObjects lastCubes newCubes
    forM_ ( zip [0..] ( Map.elems cubes ) ) $ \( i , obj ) -> do

      let model = mkTransformation (obj ^. objOrientation) (obj ^. objPosition)

      drawEntity model projectionView i cube

  withVAO ( vAO hand ) $ do

    glEnable GL_CULL_FACE
    glCullFace GL_BACK

    -- Draw the local player's hands
    handPoses <- use $ wldPlayer . plrHandPoses
    forM_ handPoses $ \(Pose posit orient) -> do
      let model = mkTransformation orient posit
      drawEntity model projectionView 0 cube 




    -- Draw all remote players' bodies and hands
    players <- use $ wldPlayers . to Map.toList
    forM_ players $ \(playerID, player) -> 
      when (playerID /= localPlayerID) $ do
        forM_ (player ^. plrHandPoses) $ \handPose -> do
          drawEntity (poseToMatrix handPose) projectionView 0 cube 


  withVAO ( vAO face ) $ do
    -- Draw all remote players' bodies and hands
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
  glUniform3f ( unUniformLocation  planeCamU )
      ( eyePos ^. _x )
      ( eyePos ^. _y )
      ( eyePos ^. _z )

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

  let light1 = uLight1 ( uniforms anEntity )
  glUniform3f ( unUniformLocation  light1 ) ( l1 ^. _x ) ( l1 ^. _y ) ( l1 ^. _z )

  let light2 = uLight2 ( uniforms anEntity )
  glUniform3f ( unUniformLocation  light2 ) ( l2 ^. _x ) ( l2 ^. _y ) ( l2 ^. _z )

  let light3 = uLight3 ( uniforms anEntity )
  glUniform3f ( unUniformLocation  light3 ) ( l3 ^. _x ) ( l3 ^. _y ) ( l3 ^. _z )

  let light4 = uLight4 ( uniforms anEntity )
  glUniform3f ( unUniformLocation  light4 ) ( l4 ^. _x ) ( l4 ^. _y ) ( l4 ^. _z )






drawLights :: MonadIO m => Entity Uniforms
           -> M44 GLfloat
           -> V3 GLfloat
           -> V3 GLfloat
           -> V3 GLfloat
           -> V3 GLfloat
           -> m ()
drawLights anEntity projectionView l1 l2 l3 l4 = do

  useProgram (program anEntity )

  withVAO ( vAO anEntity  ) $ do

    glEnable GL_CULL_FACE
    glCullFace GL_BACK

    let model0 = mkTransformation 
            ( axisAngle ( V3 1 0 0 ) 0.0 )
            l1

    drawEntity model0 projectionView 0 anEntity

    let model1 = mkTransformation 
            ( axisAngle ( V3 1 0 0 ) 0.0 )
            l2

    drawEntity model1 projectionView 1 anEntity

    let model2 = mkTransformation 
            ( axisAngle ( V3 1 0 0 ) 0.0 )
            l3

    drawEntity model2 projectionView 2 anEntity

    let model3 = mkTransformation 
            ( axisAngle ( V3 1 0 0 ) 0.0 )
            l4

    drawEntity model3 projectionView 3 anEntity






drawEntity :: MonadIO m => M44 GLfloat -> M44 GLfloat -> GLfloat -> Entity Uniforms -> m ()
drawEntity model projectionView drawID anEntity = do 

  let Uniforms{..} = uniforms anEntity

  uniformM44 uMVP ( projectionView !*! model)
  uniformM44 uInverseModel (fromMaybe model (inv44 model))
  uniformM44 uModel model

  let dID = uID 
  glUniform1f ( unUniformLocation dID ) drawID

  let vc = vertCount ( geometry anEntity ) 
  glDrawElements GL_TRIANGLES ( vc ) GL_UNSIGNED_INT nullPtr



addCube :: (MonadIO m, MonadState World m, MonadRandom m) => Transceiver Op -> V3 GLfloat -> Quaternion GLfloat -> m ()
addCube transceiver posit orient = do
  -- Spawn a cube at the player's position and orientation
  instruction <- do
    objID <- getRandom
    return $ UpdateObject objID (Object posit orient 0.25)
  
  interpret instruction


  writeTransceiver transceiver $ Reliable instruction
  return ()

  
  
