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


bufferElementData :: [GLuint] -> IO ElementArrayBuffer
bufferElementData values  = do

  buffer <- ElementArrayBuffer <$> overPtr ( glGenBuffers 1 )

  withElementArrayBuffer buffer $ do

    let valuesSize = fromIntegral ( sizeOf ( undefined :: GLuint ) * length values )

    withArray values $ 
      \valuesPtr ->
        glBufferData GL_ELEMENT_ARRAY_BUFFER valuesSize ( castPtr valuesPtr ) GL_STATIC_DRAW

  return buffer

