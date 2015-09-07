{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Resources where
import Graphics.GL.Pal
import Graphics.GL
import Linear
import Data.Data
import Control.Lens

data Uniforms = Uniforms
  { uModelViewProjection :: UniformLocation (M44 GLfloat)
  , uInverseModel        :: UniformLocation (M44 GLfloat)
  , uModel               :: UniformLocation (M44 GLfloat)
  , uCamera              :: UniformLocation (V3  GLfloat)
  , uLight1              :: UniformLocation (V3  GLfloat)
  , uLight2              :: UniformLocation (V3  GLfloat)
  , uLight3              :: UniformLocation (V3  GLfloat)
  , uLight4              :: UniformLocation (V3  GLfloat)
  , uParameter1          :: UniformLocation GLfloat
  , uParameter2          :: UniformLocation GLfloat
  , uParameter3          :: UniformLocation GLfloat
  , uParameter4          :: UniformLocation GLfloat
  , uParameter5          :: UniformLocation GLfloat
  , uParameter6          :: UniformLocation GLfloat
  , uID                  :: UniformLocation GLfloat
  , uTime                :: UniformLocation GLfloat
  , uTick                :: UniformLocation GLfloat
  } deriving (Data)

data Resources = Resources
  { plane :: Shape Uniforms
  , cube  :: Shape Uniforms
  , light :: Shape Uniforms
  , hand  :: Shape Uniforms
  , face  :: Shape Uniforms
  }

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
  

  -- Set up our cube resources
  cubeProg   <- createShaderProgram "src/shaders/raytrace.vert" "src/shaders/spaceCube.frag"
  cubeGeo    <- cubeGeometry ( 0.5 :: V3 GLfloat ) ( V3 1 1 1 )

   -- Set up our cube resources (face testing)
  --cubeProg   <- createShaderProgram "src/shaders/raytrace.vert" "src/shaders/face.frag"
  ----cubeGeo    <- cubeGeometry ( 0.5 :: V3 GLfloat ) ( V3 1 1 1 )
  --cubeGeo    <- cubeGeometry ( V3 0.5 0.7 0.1 ) ( V3 1 1 1 )

  -- Set up our light resources
  lightProg  <- createShaderProgram "src/shaders/raytrace.vert" "src/shaders/spaceCube.frag"
  lightGeo   <- cubeGeometry ( V3 0.1 0.1 0.1 ) ( V3 1 1 1 )

  {-
  -- Set up our plane resources
  planeProg  <- createShaderProgram "src/shaders/plane.vert" "src/shaders/plane.frag"
  planeGeo   <- cubeGeometry ( V3 20 20 20 ) ( V3 1 1 1 )
  -}

  -- Set up our cube resources
  planeProg  <- createShaderProgram "src/shaders/raytrace.vert" "src/shaders/background1.frag"
  planeGeo   <- cubeGeometry ( V3 3 3 3 ) ( V3 1 1 1 )

  -- Set up our hand resources
  handProg   <- createShaderProgram "src/shaders/raytrace.vert" "src/shaders/spaceCube.frag"
  handGeo    <- cubeGeometry handDimensions ( V3 1 1 1 )

  -- Set up face cube resources
  faceProg   <- createShaderProgram "src/shaders/raytrace.vert" "src/shaders/face.frag"
  faceGeo    <- cubeGeometry ( V3 0.5 0.7 0.1 ) ( V3 1 1 1 )

  plane      <- makeShape planeGeo planeProg 
  cube       <- makeShape cubeGeo  cubeProg 
  light      <- makeShape lightGeo lightProg 
  hand       <- makeShape handGeo  handProg 
  face       <- makeShape faceGeo  faceProg 

  return Resources{..}
