{-# LANGUAGE RecordWildCards #-}

module Pal.Uniforms where
import Graphics.GL

import Pal.Types
import Pal.Shader

import Control.Monad.Trans
import Foreign




import Linear
import Data.Foldable

assignUniforms :: Program -> IO Uniforms
assignUniforms program = do 

  uMVP            <- getShaderUniform program "uModelViewProjection"
  uInverseModel   <- getShaderUniform program "uInverseModel"
  uModel          <- getShaderUniform program "uModel"
  uCamera         <- getShaderUniform program "uCamera"
  uLight1         <- getShaderUniform program "uLight1"
  uLight2         <- getShaderUniform program "uLight2"
  uLight3         <- getShaderUniform program "uLight3"
  uLight4         <- getShaderUniform program "uLight4"
  uID             <- getShaderUniform program "uID"

  return Uniforms{..}

uniformM44 :: MonadIO m => UniformLocation -> M44 GLfloat -> m ()
uniformM44 uniform matrix = liftIO $ do
    let mvpUniformLoc = fromIntegral (unUniformLocation uniform)
    withArray (concatMap toList (transpose matrix)) (\matrixPtr ->
        glUniformMatrix4fv mvpUniformLoc 1 GL_FALSE matrixPtr)

uniformM33 :: MonadIO m => UniformLocation -> M33 GLfloat -> m ()
uniformM33 uniform matrix = liftIO $ do
    let mvpUniformLoc = fromIntegral (unUniformLocation uniform)
    withArray (concatMap toList (transpose matrix)) (\matrixPtr ->
        glUniformMatrix3fv mvpUniformLoc 1 GL_FALSE matrixPtr)
