module Pal.AssignAttribute where

import Foreign
import Graphics.GL
import Graphics.GL.Pal

assignAttribute :: Storable a =>
                   GLProgram -> String -> GLint -> [a] -> IO ()

assignAttribute program attributeName attributeLength values = do

  -- Gets the attribute for the program we have passed in
  attribute <- getShaderAttribute program attributeName

  vaoBuffer <- overPtr (glGenBuffers 1)

  glBindBuffer GL_ARRAY_BUFFER vaoBuffer

  let valuesSize = fromIntegral ( sizeOf ( undefined :: GLfloat ) * length values )

  withArray values $ 
    \valuesPtr ->
      glBufferData GL_ARRAY_BUFFER valuesSize ( castPtr valuesPtr ) GL_STATIC_DRAW 

  -- Describe our array to OpenGL
  glEnableVertexAttribArray ( fromIntegral ( unAttributeLocation attribute ) )

  glVertexAttribPointer
    ( fromIntegral ( unAttributeLocation attribute ) ) -- attribute
      attributeLength   -- number of elements per vertex, here (x,y,z)
      GL_FLOAT          -- the type of each element
      GL_FALSE          -- don't normalize
      0                 -- no extra data between each position
      nullPtr           -- offset of first element


