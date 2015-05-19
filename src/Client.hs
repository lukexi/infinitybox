{-# LANGUAGE FlexibleContexts, LambdaCase #-}
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

import qualified Control.Monad.Trans.Free as FT
import Instructions
import Control.Monad.Free.Binary ()
import Control.Monad.Free.FromFreeT
import Control.Monad.Free

{-
Instanced drawing.
Batched shader and VAO drawing.
-}



newPlayer :: Player
newPlayer = Player (V3 0 0 5) (axisAngle (V3 0 1 0) 0)

newWorld :: World
newWorld = World newPlayer mempty

-- | Get a view matrix for a camera at a given position and orientation
viewMatrix :: (RealFloat a, Conjugate a) => V3 a -> Quaternion a -> M44 a
viewMatrix position orientation = mkTransformation q (rotate q . negate $ position)
    where q = conjugate orientation

-- | Use the aspect ratio from the window to get a proper projection
makeProjection :: (Floating a, MonadIO m) => Window -> m (M44 a)
makeProjection win = do
    (w,h) <- getWindowSize win
    return $ perspective 45 (fromIntegral w / fromIntegral h) 0.01 1000

playerViewMat :: MonadState World m => m (M44 GLfloat)
playerViewMat = do
    playerPos    <- use $ wldPlayer . plrPosition
    playerOrient <- use $ wldPlayer . plrOrientation
    return $ viewMatrix playerPos playerOrient

applyMouseLook :: (MonadIO m, MonadState World m) => Window -> m ()
applyMouseLook win = do
    (x,y) <- getCursorPos win
    wldPlayer . plrOrientation .= axisAngle (V3 0 1 0) (-x/500)
                                * axisAngle (V3 1 0 0) (-y/500)

movePlayer :: MonadState World m => V3 GLfloat -> m ()
movePlayer vec = do
    orient <- use (wldPlayer . plrOrientation)
    wldPlayer . plrPosition += rotate orient vec

applyMovement :: (MonadIO m, MonadState World m) => Window -> m ()
applyMovement win = do
    let pos = 0.1
        neg = -pos
    whenKeyPressed win Key'W           $ movePlayer (V3 0   0   neg)
    whenKeyPressed win Key'S           $ movePlayer (V3 0   0   pos)
    whenKeyPressed win Key'A           $ movePlayer (V3 neg 0   0  )
    whenKeyPressed win Key'D           $ movePlayer (V3 pos 0   0  )
    whenKeyPressed win Key'Space       $ movePlayer (V3 0   pos 0  )
    whenKeyPressed win Key'LeftControl $ movePlayer (V3 0   neg 0  )

whenKeyPressed :: MonadIO m => Window -> Key -> m () -> m ()
whenKeyPressed win key action = getKey win key >>= \case
    KeyState'Pressed -> action
    _                -> return ()

sendInstrs :: (MonadIO m) => Socket -> Instructions -> m Int
sendInstrs s = sendB s

main :: IO ()
main = asClient $ \s -> do

    stdGen <- getStdGen

    -- | Create a UDP receive thread
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

    
    void . flip runRandT stdGen . flip runStateT newWorld . whileWindow win $ do
        swapBuffers win

        -- Handle network events
        readChanAll receiveChan $ \instructions -> do
            iterM interpret instructions

        -- Handle key events
        processEvents events $ \e -> do
            keyDown Key'Enter e (addCube s)

        -- Handle mouse events
        applyMouseLook win

        -- Handle movement events
        applyMovement win

        -- Render the scene
        render win cube

render :: (MonadIO m, MonadState World m) => Window -> Cube -> m ()
render win cube = do
    glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)

    projMat <- makeProjection win
    viewMat <- playerViewMat

    let viewProj = projMat !*! viewMat

    -- Begin cube batch
    useProgram (cubeShader cube)
    glBindVertexArray (unVertexArrayObject (cubeVAO cube))

    cubes <- use wldCubes
    forM_ cubes $ \obj -> do
        let model = mkTransformation (obj ^. objOrientation) (obj ^. objPosition)
        uniformM44 (cubeUniformMVP cube) (viewProj !*! model)
        glDrawElements GL_TRIANGLES (cubeIndexCount cube) GL_UNSIGNED_INT nullPtr

getRandomR' :: (MonadTrans t, Random a, MonadRandom m) => (a, a) -> t m a
getRandomR' = lift . getRandomR
getRandom' :: (MonadTrans t, Random a, MonadRandom m) => t m a
getRandom' = lift getRandom

makeOps :: MonadRandom m => FT.FreeT Op m ()
makeOps = do
    position <- V3 
        <$> getRandomR' (-5, 5)
        <*> getRandomR' (-5, 5)
        <*> getRandomR' (-2, -20)
    axis <- V3
        <$> getRandomR' (0, 1)
        <*> getRandomR' (0, 1)
        <*> getRandomR' (0, 1)
    angle' <- getRandomR' (0, 2*pi)
    let orientation = axisAngle axis angle'
        object = Object position orientation
    objID <- getRandom'
    update objID object
    echo (show object)

interpret :: (MonadIO m, MonadState World m) => Op t -> m ()
interpret (Echo s _)           = liftIO $ putStrLn s
interpret (Update objID obj _) = wldCubes . at objID ?= obj
interpret (Connect name _)        = liftIO . putStrLn $ name ++ " connected"

addCube :: (MonadIO m, MonadState World m, MonadRandom m) => Socket -> m ()
addCube s = do

    instructions <- fromFreeT makeOps
    
    iterM interpret instructions

    _bytesSent <- sendInstrs s instructions
    return ()

    
    
