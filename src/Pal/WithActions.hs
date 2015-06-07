module Pal.WithActions where

import Graphics.GL
import Graphics.GL.Pal

withVAO aVAO action = do

  glBindVertexArray ( unVertexArrayObject aVAO )

  action

  glBindVertexArray 0