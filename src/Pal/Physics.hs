module Pal.Physics where

import Linear

import Data.Default 

import Physics.Bullet

addBox dynamicsWorld height = do
  addStaticPlane dynamicsWorld def { rotation = axisAngle ( V3 1 0 0 ) ((-pi)/2) , yPos = height } 
  addStaticPlane dynamicsWorld def { rotation = axisAngle ( V3 1 0 0 ) (( pi)/2) , yPos = height } 
  addStaticPlane dynamicsWorld def { rotation = axisAngle ( V3 0 1 0 ) ((-pi)/2) , yPos = height }
  addStaticPlane dynamicsWorld def { rotation = axisAngle ( V3 0 1 0 ) (( pi)/2) , yPos = height } 
  addStaticPlane dynamicsWorld def { rotation = axisAngle ( V3 0 1 0 ) (0 )      , yPos = height } 
  addStaticPlane dynamicsWorld def { rotation = axisAngle ( V3 0 1 0 ) (pi)      , yPos = height } 



