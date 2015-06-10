module Randoms where

import System.Random
import Control.Monad.Random
import Control.Monad.Trans
import Types
import Linear
import Control.Monad.Free.FromFreeT

-- MonadRandom doesn't seem to have the autolifting machinery, so we define manual versions
getRandomR' :: (MonadTrans t, Random a, MonadRandom m) => (a, a) -> t m a
getRandomR' = lift . getRandomR

getRandom' :: (MonadTrans t, Random a, MonadRandom m) => t m a
getRandom' = lift getRandom

randomCube :: MonadRandom m => m Instructions
randomCube = fromFreeT $ do
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
        object = Object position orientation 0.05
    objID <- getRandom'
    updateObject objID object