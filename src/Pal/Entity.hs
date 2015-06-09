{-# LANGUAGE RecordWildCards #-}

module Pal.Entity where
  
import Graphics.GL

import Pal.Types
import Pal.Uniforms
import Pal.WithActions
import Pal.AssignAttribute

entity :: Geometry -> Program -> IO Entity
entity geometry program = do

  -- Setup a VAO
  vAO <- VertexArrayObject <$> overPtr ( glGenVertexArrays 1 )

  withVAO vAO $ do

    withArrayBuffer ( positions   geometry ) $ assignAttribute program "aPosition" 3 
    withArrayBuffer ( normals     geometry ) $ assignAttribute program "aNormal"   3 
    withArrayBuffer ( tangents    geometry ) $ assignAttribute program "aTangent"  3
    withArrayBuffer ( uvs         geometry ) $ assignAttribute program "aUV"       2 

    glBindBuffer GL_ELEMENT_ARRAY_BUFFER ( unElementArrayBuffer ( indices geometry ))


  uniforms <- assignUniforms program

  return Entity{..}


  

