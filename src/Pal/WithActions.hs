module Pal.WithActions where

import Graphics.GL
import Control.Monad.Trans

import Pal.Types

withVAO :: MonadIO m => VertexArrayObject -> m a -> m ()
withVAO aVAO action = do

  glBindVertexArray ( unVertexArrayObject aVAO )

  _ <- action

  glBindVertexArray 0



withArrayBuffer buffer action = do

  glBindBuffer GL_ARRAY_BUFFER ( unArrayBuffer buffer )

  action
  
  glBindBuffer GL_ARRAY_BUFFER 0


withElementArrayBuffer buffer action = do

  glBindBuffer GL_ELEMENT_ARRAY_BUFFER ( unElementArrayBuffer buffer )

  action
  
  glBindBuffer GL_ELEMENT_ARRAY_BUFFER 0