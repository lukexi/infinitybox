{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Themes where
import Graphics.GL.Pal
import Graphics.GL
import Linear
import Control.Lens
import Types


loadThemes :: IO Themes
loadThemes = do
  
  let vRainbow = Visuals
        { _roomShader  = "src/shaders/background1.frag"
        , _logoShader  = "src/shaders/logo.frag"
        , _cubeShader  = "src/shaders/slugCube.frag"
        , _faceShader  = "src/shaders/face.frag"
        , _handShader  = "src/shaders/slugCube.frag"
        , _lightShader = "src/shaders/slugCube.frag"
        , _vertShader  = "src/shaders/raytrace.vert"
        }

      vAO = Visuals
        { _roomShader  = "src/shaders/aoBG.frag"
        , _logoShader  = "src/shaders/logo.frag"
        , _cubeShader  = "src/shaders/aoCube.frag"
        , _faceShader  = "src/shaders/face.frag"
        , _handShader  = "src/shaders/aoCube.frag"
        , _lightShader = "src/shaders/aoCube.frag"
        , _vertShader  = "src/shaders/raytrace.vert"
        }



  cubeGeo    <- cubeGeometry ( V3 1 1 1 ) ( V3 1 1 1 )
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
          <*> ( makeShape cubeGeo  =<< csp ( vRainbow ^. vertShader ) ( vRainbow ^. logoShader  ) )
        )
    <*> (Resources
          <$> ( makeShape roomGeo  =<< csp ( vAO ^. vertShader ) ( vAO ^. roomShader  ) )
          <*> ( makeShape cubeGeo  =<< csp ( vAO ^. vertShader ) ( vAO ^. cubeShader  ) )
          <*> ( makeShape lightGeo =<< csp ( vAO ^. vertShader ) ( vAO ^. lightShader ) )
          <*> ( makeShape handGeo  =<< csp ( vAO ^. vertShader ) ( vAO ^. handShader  ) )
          <*> ( makeShape faceGeo  =<< csp ( vAO ^. vertShader ) ( vAO ^. faceShader  ) )
          <*> ( makeShape cubeGeo  =<< csp ( vAO ^. vertShader ) ( vAO ^. logoShader  ) )
        )


  return theme
