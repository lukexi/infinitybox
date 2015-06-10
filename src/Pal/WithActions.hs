module Pal.WithActions where

import Graphics.GL
import Control.Monad.Trans

import Pal.Types

withVAO :: MonadIO m => VertexArrayObject -> m a -> m ()
withVAO aVAO action = do

  glBindVertexArray ( unVertexArrayObject aVAO )

  _ <- action

  glBindVertexArray 0


withArrayBuffer :: MonadIO m => ArrayBuffer -> m a -> m ()
withArrayBuffer buffer action = do

  glBindBuffer GL_ARRAY_BUFFER ( unArrayBuffer buffer )

  _ <- action
  
  glBindBuffer GL_ARRAY_BUFFER 0

withElementArrayBuffer :: MonadIO m => ElementArrayBuffer -> m a -> m ()
withElementArrayBuffer buffer action = do

  glBindBuffer GL_ELEMENT_ARRAY_BUFFER ( unElementArrayBuffer buffer )

  _ <- action
  
  glBindBuffer GL_ELEMENT_ARRAY_BUFFER 0