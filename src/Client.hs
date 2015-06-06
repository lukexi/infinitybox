{-# LANGUAGE FlexibleContexts, LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
import Graphics.UI.GLFW.Pal

import Halive.Utils
import Graphics.GL.Pal
import Graphics.GL
import Linear


import Control.Monad
import Control.Monad.State
import System.Random
import Control.Lens

import Control.Monad.Random

import GELP.WithActions

import Geo.CubeInfo
import Geo.PlaneInfo
import Geo.Geometry

import Network.Sox
import Network.ReceiveChan

import Types
import Randoms
import Movement
import Control.Monad.Free.Binary ()
import Control.Monad.Free.FromFreeT
import qualified Data.Map as Map
import Data.Maybe

main :: IO ()
main = asClient $ \s -> do

    stdGen <- getStdGen

    -- Create a UDP receive thread
    receiveChan <- makeReceiveChan s

    _bytesSent <- sendInstrs s (compile stdGen (connect "player"))

    -- Reacquire our window
    (win, events) <- reacquire 0 $ createWindow "R2" 1000 618

    -- Lock the cursor for mouselook
    setCursorInputMode win CursorInputMode'Disabled

    -- Set up our cube resources
    cubeProg <- createShaderProgram "src/Geo/cube.vert" "src/Geo/cube.frag"
    cubeGeometry <- initCubeGeometry cubeProg

    -- Set up our cube resources
    planeProg <- createShaderProgram "src/Geo/cube.vert" "src/Geo/cube.frag"
    planeGeometry <- initPlaneGeometry planeProg

    -- Set up GL state
    glEnable GL_DEPTH_TEST
    glClearColor 0 0 0.1 1

    -- Begin game loop
    void . flip runRandT stdGen . flip runStateT newWorld . whileWindow win $ do
        swapBuffers win

        -- Update interpolation buffer
        wldLastCubes <~ use wldCubes

        -- Handle network events
        readChanAll receiveChan interpret

        -- Handle key events
        processEvents events $ \e -> do
            closeOnEscape win e
            keyDown Key'E e (addCube s)
            keyDown Key'F e (setCursorInputMode win CursorInputMode'Disabled)
            keyDown Key'G e (setCursorInputMode win CursorInputMode'Normal)

        -- Handle mouse events
        isFocused <- getWindowFocused win
        when isFocused $ applyMouseLook win

        -- Handle movement events
        applyMovement win

        -- Render the scene
        render win cubeGeometry planeGeometry

render :: ( MonadIO m, MonadState World m ) => Window -> Geometry -> Geometry -> m ()
render win cubeGeometry planeGeometry = do
    glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)

    projMat <- makeProjection win
    viewMat <- playerViewMat

    let viewProj = projMat !*! viewMat

 
    
    newCubes  <- use wldCubes
    lastCubes <- use wldLastCubes

    cameraPos <- use (wldPlayer . plrPosition)

    -- Begin cube batch
    useProgram (program cubeGeometry)

    withVAO (vAO cubeGeometry) $ do

        -- let cubes = lerp 0.5 newCubes lastCubes
        let cubes = Map.unionWith interpolateObjects lastCubes newCubes
        forM_ cubes $ \obj -> do
            let model = mkTransformation (obj ^. objOrientation) (obj ^. objPosition)
            uniformM44 ( uMVP cubeGeometry ) (viewProj !*! model)

            glUniform3f (unUniformLocation ( uCamera cubeGeometry )) 
                        (cameraPos ^. _x)
                        (cameraPos ^. _y)
                        (cameraPos ^. _z)
            uniformM44 ( uInverseModel cubeGeometry ) (fromMaybe model (inv44 model))
            uniformM44 ( uModel cubeGeometry ) model
            glDrawArrays GL_TRIANGLES 0 ( vertCount cubeGeometry )


    useProgram (program planeGeometry)

    withVAO (vAO planeGeometry) $ do

        let model = mkTransformation 
                        ( axisAngle ( V3 1 0 0 ) ((-pi)/2) )
                        ( V3 0 0 0 )

        uniformM44 ( uMVP planeGeometry ) ( viewProj !*! model )
        glUniform3f (unUniformLocation ( uCamera planeGeometry )) 
                    (cameraPos ^. _x)
                    (cameraPos ^. _y)
                    (cameraPos ^. _z)
        uniformM44 ( uInverseModel planeGeometry ) (fromMaybe model (inv44 model))
        uniformM44 ( uModel planeGeometry ) model

        glDrawArrays GL_TRIANGLES 0 ( vertCount planeGeometry )



addCube :: (MonadIO m, MonadState World m, MonadRandom m) => Socket -> m ()
addCube s = do

    -- Spawn a cube at the player's position and orientation
    instructions <- fromFreeT $ do
        playerPos <- use (wldPlayer . plrPosition)
        playerRot <- use (wldPlayer . plrOrientation)
        let spawnPoint = rotate playerRot (V3 0 1 0) + playerPos
            object = Object spawnPoint playerRot
        
        objID <- getRandom'
        update objID object

    -- instructions <- randomCube
    
    interpret instructions

    _bytesSent <- sendInstrs s instructions
    return ()

    
    
