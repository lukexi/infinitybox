module Physics where

import Linear

import Physics.Bullet
import Control.Monad.Trans

addRoom :: MonadIO m => DynamicsWorld -> Float -> m ()
addRoom dynamicsWorld height = do
  _ <- addStaticPlane dynamicsWorld (RigidBodyID 0) mempty { pcRotation = axisAngle ( V3 1 0 0 ) ((-pi)/2) , pcYPos = ( height * 0.5 ) } 
  _ <- addStaticPlane dynamicsWorld (RigidBodyID 0) mempty { pcRotation = axisAngle ( V3 1 0 0 ) (( pi)/2) , pcYPos = ( height * 0.5 ) } 
  _ <- addStaticPlane dynamicsWorld (RigidBodyID 0) mempty { pcRotation = axisAngle ( V3 0 1 0 ) ((-pi)/2) , pcYPos = ( height * 0.5 ) }
  _ <- addStaticPlane dynamicsWorld (RigidBodyID 0) mempty { pcRotation = axisAngle ( V3 0 1 0 ) (( pi)/2) , pcYPos = ( height * 0.5 ) } 
  _ <- addStaticPlane dynamicsWorld (RigidBodyID 0) mempty { pcRotation = axisAngle ( V3 0 1 0 ) (0 )      , pcYPos = ( height * 0.5 ) } 
  _ <- addStaticPlane dynamicsWorld (RigidBodyID 0) mempty { pcRotation = axisAngle ( V3 0 1 0 ) (pi)      , pcYPos = ( height * 0.5 ) }
  return ()


