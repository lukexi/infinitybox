{-# LANGUAGE FlexibleContexts, LambdaCase, RecordWildCards #-}
import Graphics.UI.GLFW.Pal

import Halive.Utils
import Graphics.GL.Pal
import Graphics.GL
import Linear


import Control.Monad
import Control.Monad.State
import System.Random
import Control.Lens
import Foreign (nullPtr)

import Control.Monad.Random

import Geo.Cube

import Network.Sox
import Network.ReceiveChan

import Types
import Randoms
import Movement
import Control.Monad.Free.Binary ()
import Control.Monad.Free.FromFreeT

main :: IO ()
main = asClient $ \s -> do

    stdGen <- getStdGen

    -- Create a UDP receive thread
    receiveChan <- makeReceiveChan s

    _bytesSent <- sendInstrs s (compile stdGen (connect "player"))

    -- Reacquire our window
    (win, events) <- reacquire 0 $ createWindow "R2" 640 480

    -- Set up our cube resources
    cubeProg <- createShaderProgram "src/Geo/cube.vert" "src/Geo/cube.frag"
    cube     <- makeCube cubeProg

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
            keyDown Key'Enter e (addCube s)

        -- Handle mouse events
        applyMouseLook win

        -- Handle movement events
        applyMovement win

        -- Render the scene
        render win cube

render :: (MonadIO m, MonadState World m) => Window -> Cube -> m ()
render win Cube{..} = do
    glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)

    projMat <- makeProjection win
    viewMat <- playerViewMat

    let viewProj = projMat !*! viewMat

    -- Begin cube batch
    useProgram cubeShader
    glBindVertexArray (unVertexArrayObject cubeVAO)

    newCubes <- use wldCubes
    lastCubes <- use wldLastCubes
    let cubes = lerp 0.5 newCubes lastCubes
    forM_ cubes $ \obj -> do
        let model = mkTransformation (obj ^. objOrientation) (obj ^. objPosition)
        uniformM44 cubeUniformMVP (viewProj !*! model)
        glDrawElements GL_TRIANGLES cubeIndexCount GL_UNSIGNED_INT nullPtr


addCube :: (MonadIO m, MonadState World m, MonadRandom m) => Socket -> m ()
addCube s = do

    -- Spawn a cube at the player's position and orientation
    instructions <- fromFreeT $ do
        playerPos <- use (wldPlayer . plrPosition)
        playerRot <- use (wldPlayer . plrOrientation)
        let object = Object playerPos playerRot
        
        objID <- getRandom'
        update objID object

    -- instructions <- randomCube
    
    interpret instructions

    _bytesSent <- sendInstrs s instructions
    return ()

    
    
