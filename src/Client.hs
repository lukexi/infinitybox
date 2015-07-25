{-# LANGUAGE FlexibleContexts, LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
--module Client where
import Graphics.UI.GLFW.Pal

import Graphics.GL
import Graphics.Oculus
import Linear

import System.Hardware.Hydra

import Control.Monad
import Control.Monad.State
import System.Random
import Control.Lens hiding (view)
import qualified Data.Map as Map
import Data.Maybe
import Control.Monad.Random

import Graphics.GL.Pal2

import Sound.Pd1

import Network.UDP.Pal

import Types
import Movement
import Resources


enableVR :: Bool
enableVR = False
--enableVR = True

enableHydra :: Bool
--enableHydra = False
enableHydra = True

main :: IO ()
main = do
  -- Set up Hydra
  sixenseBase <- if enableHydra then Just <$> initSixense else return Nothing
  
  _patch <- makePatch "src/world"
  openALSources <- getPdSources

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
  stdGen   <- getStdGen
  
  -- Connect to the server
  -- Initial connection to the server
  playerID <- randomName
  writeTransceiver transceiver $ Reliable (Connect playerID)


  resources@Resources{..} <- loadResources


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
    hands <- maybe (return []) (getHands) sixenseBase

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

    -- Send player position
    player <- use wldPlayer
    writeTransceiver transceiver $ Reliable $ UpdatePlayer playerID player

    -- Render to OpenAL
    -- TODO add wldPlayer.plrHeadPose
    liftIO . alListenerPosition    =<< use (wldPlayer . plrPose . posPosition . to (fmap realToFrac))
    liftIO . alListenerOrientation =<< use (wldPlayer . plrPose . posOrientation . to (fmap realToFrac))
    liftIO $ forM_ (zip openALSources handWorldPoses) $ \(sourceID, Pose posit orient) -> do
      alSourcePosition    sourceID (fmap realToFrac posit)
      --alSourceOrientation sourceID (fmap realToFrac orient)


    -- Render the scene
    case maybeRenderHMD of

      Nothing        -> renderFlat window    resources
      Just renderHMD -> renderVR   renderHMD resources





renderVR :: (MonadIO m, MonadState World m) 
         => RenderHMD -> Resources -> m ()
renderVR renderHMD resources = do

  renderHMDFrame renderHMD $ \eyePoses -> do

    glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)

    view <- playerViewMat

    renderHMDEyes renderHMD eyePoses $ \projection eyeView -> do

      let finalView = eyeView !*! view 

      render resources projection finalView 

renderFlat :: (MonadIO m, MonadState World m) 
           => Window -> Resources -> m ()
renderFlat win resources = do

  glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
  
  projection  <- makeProjection win
  view        <- playerViewMat

  render resources projection view

  swapBuffers win

poseToMatrix :: Pose -> M44 GLfloat
poseToMatrix (Pose posit orient) = mkTransformation orient posit

render :: (MonadIO m, MonadState World m) 
       => Resources
       -> M44 GLfloat
       -> M44 GLfloat
       -> m ()
render Resources{..} projection view = do


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
  uniformV3 cam eyePos

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




    -- Draw all remote players' hands
    players <- use $ wldPlayers . to Map.toList
    forM_ players $ \(playerID, player) -> 
      when (playerID /= localPlayerID) $ do
        forM_ (player ^. plrHandPoses) $ \handPose -> do
          drawEntity (poseToMatrix handPose) projectionView 0 cube 

  -- Draw all remote players' heads 
  -- (we don't draw the local player's head)
  withVAO ( vAO face ) $ do
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

  let light1 = uLight1 ( uniforms anEntity )
  uniformV3 light1 l1

  let light2 = uLight2 ( uniforms anEntity )
  uniformV3 light2 l2

  let light3 = uLight3 ( uniforms anEntity )
  uniformV3 light3 l3

  let light4 = uLight4 ( uniforms anEntity )
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

  uniformM44 uModelViewProjection ( projectionView !*! model)
  uniformM44 uInverseModel        (fromMaybe model (inv44 model))
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
  putStrLnIO "Print creating a cube sir"
  
  interpret instruction


  writeTransceiver transceiver $ Reliable instruction
  return ()

  
  
