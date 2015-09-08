{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Resources where
import Graphics.GL.Pal
import Graphics.GL
import Linear
import Data.Data
import Control.Lens
import Types

-- Offset the lights to be on the end of the wands
handLightOffset :: V3 GLfloat
handLightOffset = handOffset * 2

-- Offset the hand model outward to feel like wands rather than batons
handOffset :: V3 GLfloat
handOffset = V3 0 0 (-(handDimensions ^. _z) / 2)

handDimensions :: V3 GLfloat
handDimensions = V3 0.05 0.05 0.5

loadResources :: IO Resources
loadResources = do
  
  let visuals = Visuals
        { _roomShader  = "src/shaders/background1.frag"
        , _cubeShader  = "src/shaders/aoCube.frag"
        , _faceShader  = "src/shaders/face.frag"
        , _handShader  = "src/shaders/spaceCube.frag"
        , _lightShader = "src/shaders/spaceCube.frag"
        , _vertShader  = "src/shaders/raytrace.vert"
        }


  -- Set up our cube resources
  cubeProg   <- createShaderProgram ( visuals ^. vertShader )  ( visuals ^. cubeShader )
  cubeGeo    <- cubeGeometry ( 0.5 :: V3 GLfloat ) ( V3 1 1 1 )

  -- Set up our light resources
  lightProg  <- createShaderProgram ( visuals ^. vertShader ) ( visuals ^. lightShader )
  lightGeo   <- cubeGeometry ( V3 0.1 0.1 0.1 ) ( V3 1 1 1 )

  -- Set up our cube resources
  planeProg  <- createShaderProgram ( visuals ^. vertShader ) ( visuals ^. roomShader )
  planeGeo   <- cubeGeometry ( V3 3 3 3 ) ( V3 1 1 1 )

  -- Set up our hand resources
  handProg   <- createShaderProgram ( visuals ^. vertShader ) ( visuals ^. handShader )
  handGeo    <- cubeGeometry handDimensions ( V3 1 1 1 )

  -- Set up face cube resources
  faceProg   <- createShaderProgram ( visuals ^. vertShader ) ( visuals ^. faceShader )
  faceGeo    <- cubeGeometry ( V3 0.5 0.7 0.1 ) ( V3 1 1 1 )

  plane      <- makeShape planeGeo planeProg 
  cube       <- makeShape cubeGeo  cubeProg 
  light      <- makeShape lightGeo lightProg 
  hand       <- makeShape handGeo  handProg 
  face       <- makeShape faceGeo  faceProg 

  return Resources{..}
