{-# LANGUAGE RecordWildCards #-}
module Resources where
import Types
import Graphics.GL.Pal2
import Graphics.GL
import Linear

data Resources = Resources
  { plane :: Entity Uniforms
  , cube  :: Entity Uniforms
  , light :: Entity Uniforms
  , hand  :: Entity Uniforms
  , face  :: Entity Uniforms
  }

loadResources :: IO Resources
loadResources = do
  -- Set up our cube resources
  cubeProg   <- createShaderProgram "src/shaders/cube.vert" "src/shaders/cube.frag"
  cubeGeo    <- cubeGeometry ( 0.5 :: V3 GLfloat ) ( V3 1 1 1 )


  -- Set up our cube resources
  lightProg  <- createShaderProgram "src/shaders/light.vert" "src/shaders/light.frag"
  lightGeo   <- cubeGeometry ( V3 0.1 0.1 0.1 ) ( V3 1 1 1 )

  -- Set up our cube resources
  planeProg  <- createShaderProgram "src/shaders/plane.vert" "src/shaders/plane.frag"
  planeGeo   <- cubeGeometry ( V3 20 20 20 ) ( V3 1 1 1 )

  -- Set up our cube resources
  handProg  <- createShaderProgram "src/shaders/plane.vert" "src/shaders/plane.frag"
  handGeo   <- cubeGeometry ( V3 0.2 0.2 1 ) ( V3 1 1 1 )

  -- Set up our cube resources
  faceProg  <- createShaderProgram "src/shaders/plane.vert" "src/shaders/plane.frag"
  faceGeo   <- cubeGeometry ( V3 0.5 0.8 0.1 ) ( V3 1 1 1 )

  plane      <- entity planeGeo planeProg 
  cube       <- entity cubeGeo  cubeProg 
  light      <- entity lightGeo lightProg 
  hand       <- entity handGeo  handProg 
  face       <- entity faceGeo  faceProg 

  return Resources{..}