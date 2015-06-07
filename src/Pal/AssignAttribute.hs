module Pal.AssignAttribute where

import Foreign
import Graphics.GL
import Pal.Types
import Pal.Shader

assignAttribute program attributeName attributeLength = do

  -- Gets the attribute for the program we have passed in
  attribute <- getShaderAttribute program attributeName

  -- Describe our array to OpenGL
  glEnableVertexAttribArray ( fromIntegral ( unAttributeLocation attribute ) )

  glVertexAttribPointer
    ( fromIntegral ( unAttributeLocation attribute ) ) -- attribute
      attributeLength   -- number of elements per vertex, here (x,y,z)
      GL_FLOAT          -- the type of each element
      GL_FALSE          -- don't normalize
      0                 -- no extra data between each position
      nullPtr           -- offset of first element


