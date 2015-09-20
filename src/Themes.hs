{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Themes where
import Graphics.GL.Pal
import Graphics.GL
import Linear
import Control.Lens
import Types
--import Resources

-- Offset the lights to be on the end of the wands
handLightOffset :: V3 GLfloat
handLightOffset = handOffset * 2

-- Offset the hand model outward to feel like wands rather than batons
handOffset :: V3 GLfloat
handOffset = V3 0 0 (-(handDimensions ^. _z) / 2)

handDimensions :: V3 GLfloat
handDimensions = V3 0.05 0.05 0.5

loadThemes :: IO Themes
loadThemes = do
  
  let vRainbow = Visuals
        { _roomShader  = "src/shaders/background1.frag"
        --, _cubeShader  = "src/shaders/tree.frag"
        , _cubeShader  = "src/shaders/logo.frag"
        , _faceShader  = "src/shaders/face.frag"
        , _handShader  = "src/shaders/slugCube.frag"
        , _lightShader = "src/shaders/slugCube.frag"
        , _vertShader  = "src/shaders/raytrace.vert"
        }

      vAO = Visuals
        { _roomShader  = "src/shaders/aoBG.frag"
        , _cubeShader  = "src/shaders/aoCube.frag"
        , _faceShader  = "src/shaders/face.frag"
        , _handShader  = "src/shaders/aoCube.frag"
        , _lightShader = "src/shaders/aoCube.frag"
        , _vertShader  = "src/shaders/raytrace.vert"
        }



  cubeGeo    <- cubeGeometry ( 0.5 :: V3 GLfloat ) ( V3 1 1 1 )
  lightGeo   <- cubeGeometry ( V3 0.001 0.001 0.001 ) ( V3 1 1 1 )
  roomGeo    <- cubeGeometry ( V3 3 3 3 ) ( V3 1 1 1 )
  handGeo    <- cubeGeometry handDimensions ( V3 1 1 1 )
  faceGeo    <- cubeGeometry ( V3 0.5 0.7 0.1 ) ( V3 1 1 1 )


  let csp = createShaderProgram


  theme <- Themes
    <$> (Resources
          <$> ( makeShape roomGeo  =<< csp ( vRainbow ^. vertShader ) ( vRainbow ^. roomShader  ) )
          <*> ( makeShape cubeGeo  =<< csp ( vRainbow ^. vertShader ) ( vRainbow ^. cubeShader  ) )
          <*> ( makeShape lightGeo =<< csp ( vRainbow ^. vertShader ) ( vRainbow ^. lightShader ) )
          <*> ( makeShape handGeo  =<< csp ( vRainbow ^. vertShader ) ( vRainbow ^. handShader  ) )
          <*> ( makeShape faceGeo  =<< csp ( vRainbow ^. vertShader ) ( vRainbow ^. faceShader  ) )
        )
    <*> (Resources
          <$> ( makeShape roomGeo  =<< csp ( vAO ^. vertShader ) ( vAO ^. roomShader  ) )
          <*> ( makeShape cubeGeo  =<< csp ( vAO ^. vertShader ) ( vAO ^. cubeShader  ) )
          <*> ( makeShape lightGeo =<< csp ( vAO ^. vertShader ) ( vAO ^. lightShader ) )
          <*> ( makeShape handGeo  =<< csp ( vAO ^. vertShader ) ( vAO ^. handShader  ) )
          <*> ( makeShape faceGeo  =<< csp ( vAO ^. vertShader ) ( vAO ^. faceShader  ) )
        )


  return theme
