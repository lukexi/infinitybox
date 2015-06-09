{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Movement where
import Linear
import Graphics.GL
import Graphics.UI.GLFW.Pal
import Control.Monad.State
import Control.Lens
import Types

-- | Get a view matrix for a camera at a given position and orientation
viewMatrix :: (RealFloat a, Conjugate a) => V3 a -> Quaternion a -> M44 a
viewMatrix position orientation = mkTransformation q (rotate q . negate $ position)
    where q = conjugate orientation

-- | Use the aspect ratio from the window to get a proper projection
makeProjection :: (Floating a, MonadIO m) => Window -> m (M44 a)
makeProjection win = do
    (w,h) <- getWindowSize win
    return $ perspective 45 (fromIntegral w / fromIntegral h) 0.01 100

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

-- | Move player by the given vector, 
-- rotated to be relative to their current orientation
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