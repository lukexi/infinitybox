module Pal.ArrayBuffer where

import Foreign
import Graphics.GL

import Pal.Types
import Pal.WithActions

bufferData :: [GLfloat] -> IO ArrayBuffer
bufferData values  = do

  buffer <- ArrayBuffer <$> overPtr ( glGenBuffers 1 )

  withArrayBuffer buffer $ do

    let valuesSize = fromIntegral ( sizeOf ( undefined :: GLfloat ) * length values )

    withArray values $ 
      \valuesPtr ->
        glBufferData GL_ARRAY_BUFFER valuesSize ( castPtr valuesPtr ) GL_STATIC_DRAW

  return buffer


