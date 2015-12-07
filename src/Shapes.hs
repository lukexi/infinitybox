{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Shapes where
import Graphics.GL.Pal
import Control.Lens.Extra
import Types


loadShapes :: IO (Shapes, UniformBuffer)
loadShapes = do
  
  let vRainbow = Visuals
        { _roomShader     = "shaders/slugBG.frag"
        , _logoShader     = "shaders/logo.frag"
        , _cubeShader     = "shaders/slugCube.frag"
        , _handleShader   = "shaders/pureWhite.frag"
        , _faceShader     = "shaders/face.frag"
        , _handShader     = "shaders/slugCube.frag"
        , _lightShader    = "shaders/slugCube.frag"
        , _vertShader     = "shaders/raytrace.vert"
        }

  cubeGeo    <- cubeGeometry (V3 1 1 1) (V3 1 1 1)
  lightGeo   <- cubeGeometry (V3 0.001 0.001 0.001) (V3 1 1 1)
  roomGeo    <- cubeGeometry (V3 10 10 10) (V3 1 1 1)
  handGeo    <- cubeGeometry handDimensions   (V3 1 1 1)
  handleGeo  <- cubeGeometry handleDimensions (V3 1 1 1)
  faceGeo    <- cubeGeometry (V3 0.175 0.2 0.25) (V3 1 1 1)


  uboBuffer <- bufferUniformData GL_DYNAMIC_DRAW (replicate 12 (0::GLfloat))

  let uboBindingPoint = UniformBlockBindingPoint 0
  bindUniformBufferBase uboBuffer uboBindingPoint
  
  let csp fragShader = do
        shader <- createShaderProgram (vRainbow ^. vertShader) fragShader
        -- Bind the shader's uniform buffer declaration to the correct uniform buffer object
        bindShaderUniformBuffer shader "uboData" uboBindingPoint
        return shader

  shapes <- Shapes
        <$> (makeShape roomGeo   =<< csp (vRainbow ^. roomShader  ))
        <*> (makeShape cubeGeo   =<< csp (vRainbow ^. cubeShader  ))
        <*> (makeShape lightGeo  =<< csp (vRainbow ^. lightShader ))
        <*> (makeShape handGeo   =<< csp (vRainbow ^. handShader  ))
        <*> (makeShape handleGeo =<< csp (vRainbow ^. handleShader))
        <*> (makeShape faceGeo   =<< csp (vRainbow ^. faceShader  ))
        <*> (makeShape cubeGeo   =<< csp (vRainbow ^. logoShader  ))

  return (shapes, uboBuffer)