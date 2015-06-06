module Pal.WithActions where

import Graphics.GL
import Graphics.GL.Pal
import Control.Monad.Trans

withVAO :: MonadIO m => VertexArrayObject -> m a -> m ()
withVAO aVAO action = do

  glBindVertexArray ( unVertexArrayObject aVAO )

  _ <- action

  glBindVertexArray 0