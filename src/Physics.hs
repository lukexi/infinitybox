module Physics where

import Linear

import Physics.Bullet
import Control.Monad.Trans

addBox :: MonadIO m => DynamicsWorld -> Float -> m ()
addBox dynamicsWorld height = do
  _ <- addStaticPlane dynamicsWorld mempty { rotation = axisAngle ( V3 1 0 0 ) ((-pi)/2) , yPos = height } 
  _ <- addStaticPlane dynamicsWorld mempty { rotation = axisAngle ( V3 1 0 0 ) (( pi)/2) , yPos = height } 
  _ <- addStaticPlane dynamicsWorld mempty { rotation = axisAngle ( V3 0 1 0 ) ((-pi)/2) , yPos = height }
  _ <- addStaticPlane dynamicsWorld mempty { rotation = axisAngle ( V3 0 1 0 ) (( pi)/2) , yPos = height } 
  _ <- addStaticPlane dynamicsWorld mempty { rotation = axisAngle ( V3 0 1 0 ) (0 )      , yPos = height } 
  _ <- addStaticPlane dynamicsWorld mempty { rotation = axisAngle ( V3 0 1 0 ) (pi)      , yPos = height }
  return ()


