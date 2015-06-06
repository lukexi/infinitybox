{-# LANGUAGE RecordWildCards #-}

module Geo.CubeInfo where

import Graphics.GL
import Graphics.GL.Pal
import GELP.WithActions
import Geo.AssignAttribute
import Geo.Geometry

initCubeGeometry :: GLProgram -> IO Geometry
initCubeGeometry program = do


    -----------------
    -- Cube Positions
    -----------------
    
    -- Buffer the cube vertices
    let positions = 


            -- front
            [ -1.0 ,  1.0 ,  1.0
            , -1.0 , -1.0 ,  1.0 
            ,  1.0 ,  1.0 ,  1.0

            ,  1.0 , -1.0 ,  1.0
            ,  1.0 ,  1.0 ,  1.0
            , -1.0 , -1.0 ,  1.0


            -- right
            ,  1.0 ,  1.0 ,  1.0
            ,  1.0 , -1.0 ,  1.0
            ,  1.0 ,  1.0 , -1.0

            ,  1.0 , -1.0 , -1.0
            ,  1.0 ,  1.0 , -1.0
            ,  1.0 , -1.0 ,  1.0

            -- back
            ,  1.0 ,  1.0 , -1.0
            ,  1.0 , -1.0 , -1.0
            , -1.0 ,  1.0 , -1.0

            , -1.0 , -1.0 , -1.0
            , -1.0 ,  1.0 , -1.0
            ,  1.0 , -1.0 , -1.0

            -- left
            , -1.0 ,  1.0 , -1.0
            , -1.0 , -1.0 , -1.0
            , -1.0 ,  1.0 ,  1.0

            , -1.0 , -1.0 ,  1.0
            , -1.0 ,  1.0 ,  1.0
            , -1.0 , -1.0 , -1.0


            -- top
            , -1.0 ,  1.0 , -1.0
            , -1.0 ,  1.0 ,  1.0
            ,  1.0 ,  1.0 , -1.0

            ,  1.0 ,  1.0 ,  1.0
            ,  1.0 ,  1.0 , -1.0
            , -1.0 ,  1.0 ,  1.0


            -- bottom
            , -1.0 , -1.0 ,  1.0
            , -1.0 , -1.0 , -1.0
            ,  1.0 , -1.0 , -1.0

            ,  1.0 , -1.0 ,  1.0
            ,  1.0 , -1.0 , -1.0
            , -1.0 , -1.0 ,  1.0 ] :: [GLfloat]



    -----------------
    -- Cube Normals
    -----------------

    -- Buffer the cube vertices
    let normals = 


            -- front
            [  0.0 ,  0.0 ,  1.0
            ,  0.0 ,  0.0 ,  1.0
            ,  0.0 ,  0.0 ,  1.0

            ,  0.0 ,  0.0 ,  1.0
            ,  0.0 ,  0.0 ,  1.0
            ,  0.0 ,  0.0 ,  1.0


            -- right
            ,  1.0 ,  0.0 ,  0.0
            ,  1.0 ,  0.0 ,  0.0
            ,  1.0 ,  0.0 ,  0.0

            ,  1.0 ,  0.0 ,  0.0
            ,  1.0 ,  0.0 ,  0.0
            ,  1.0 ,  0.0 ,  0.0

            -- back
            ,  0.0 ,  0.0 , -1.0
            ,  0.0 ,  0.0 , -1.0
            ,  0.0 ,  0.0 , -1.0

            ,  0.0 ,  0.0 , -1.0
            ,  0.0 ,  0.0 , -1.0
            ,  0.0 ,  0.0 , -1.0

            -- left
            , -1.0 ,  0.0 ,  0.0
            , -1.0 ,  0.0 ,  0.0
            , -1.0 ,  0.0 ,  0.0

            , -1.0 ,  0.0 ,  0.0
            , -1.0 ,  0.0 ,  0.0
            , -1.0 ,  0.0 ,  0.0


            -- top
            ,  0.0 ,  1.0 ,  0.0
            ,  0.0 ,  1.0 ,  0.0
            ,  0.0 ,  1.0 ,  0.0
            
            ,  0.0 ,  1.0 ,  0.0
            ,  0.0 ,  1.0 ,  0.0
            ,  0.0 ,  1.0 ,  0.0

            -- bottom
            ,  0.0 , -1.0 ,  0.0
            ,  0.0 , -1.0 ,  0.0
            ,  0.0 , -1.0 ,  0.0
           
            ,  0.0 , -1.0 ,  0.0
            ,  0.0 , -1.0 ,  0.0
            ,  0.0 , -1.0 ,  0.0 ] :: [GLfloat]


    -----------------
    -- Cube Tangents
    -----------------

    -- Buffer the cube vertices
    let tangents = 

            -- front
            [  1.0 ,  0.0 ,  0.0
            ,  1.0 ,  0.0 ,  0.0
            ,  1.0 ,  0.0 ,  0.0

            ,  1.0 ,  0.0 ,  0.0
            ,  1.0 ,  0.0 ,  0.0
            ,  1.0 ,  0.0 ,  0.0

            -- right
            ,  0.0 ,  0.0 , -1.0
            ,  0.0 ,  0.0 , -1.0
            ,  0.0 ,  0.0 , -1.0

            ,  0.0 ,  0.0 , -1.0
            ,  0.0 ,  0.0 , -1.0
            ,  0.0 ,  0.0 , -1.0

            -- back
            , -1.0 ,  0.0 ,  0.0
            , -1.0 ,  0.0 ,  0.0
            , -1.0 ,  0.0 ,  0.0

            , -1.0 ,  0.0 ,  0.0
            , -1.0 ,  0.0 ,  0.0
            , -1.0 ,  0.0 ,  0.0

            -- left
            ,  0.0 ,  0.0 ,  1.0
            ,  0.0 ,  0.0 ,  1.0
            ,  0.0 ,  0.0 ,  1.0

            ,  0.0 ,  0.0 ,  1.0
            ,  0.0 ,  0.0 ,  1.0
            ,  0.0 ,  0.0 ,  1.0

            -- top
            ,  1.0 ,  0.0 ,  0.0
            ,  1.0 ,  0.0 ,  0.0
            ,  1.0 ,  0.0 ,  0.0

            ,  1.0 ,  0.0 ,  0.0
            ,  1.0 ,  0.0 ,  0.0
            ,  1.0 ,  0.0 ,  0.0

            -- bottom
            , -1.0 ,  0.0 ,  0.0
            , -1.0 ,  0.0 ,  0.0
            , -1.0 ,  0.0 ,  0.0

            , -1.0 ,  0.0 ,  0.0
            , -1.0 ,  0.0 ,  0.0
            , -1.0 ,  0.0 ,  0.0 ] :: [GLfloat]



    -----------------
    -- Cube UVs
    -----------------

    -- Buffer the cube vertices
    let uvs = 

            -- front
            [  0.0 ,  1.0
            ,  0.0 ,  0.0
            ,  1.0 ,  1.0

            ,  1.0 ,  0.0 
            ,  1.0 ,  1.0 
            ,  0.0 ,  0.0 

            -- right
            ,  0.0 ,  1.0
            ,  0.0 ,  0.0
            ,  1.0 ,  1.0

            ,  1.0 ,  0.0 
            ,  1.0 ,  1.0 
            ,  0.0 ,  0.0 

            -- back
            ,  0.0 ,  1.0
            ,  0.0 ,  0.0
            ,  1.0 ,  1.0

            ,  1.0 ,  0.0 
            ,  1.0 ,  1.0 
            ,  0.0 ,  0.0 

            -- left
            ,  0.0 ,  1.0
            ,  0.0 ,  0.0
            ,  1.0 ,  1.0

            ,  1.0 ,  0.0 
            ,  1.0 ,  1.0 
            ,  0.0 ,  0.0 

            -- top
            ,  0.0 ,  1.0
            ,  0.0 ,  0.0
            ,  1.0 ,  1.0

            ,  1.0 ,  0.0 
            ,  1.0 ,  1.0 
            ,  0.0 ,  0.0 

            -- bottom
            ,  0.0 ,  1.0
            ,  0.0 ,  0.0
            ,  1.0 ,  1.0

            ,  1.0 ,  0.0 
            ,  1.0 ,  1.0 
            ,  0.0 ,  0.0 ] :: [GLfloat]


    -- Setup a VAO
    vAO <- VertexArrayObject <$> overPtr ( glGenVertexArrays 1 )

    withVAO vAO $ do
      assignAttribute program "aPosition" 3 positions
      assignAttribute program "aNormal"   3 normals
      assignAttribute program "aTangent"  3 tangents
      assignAttribute program "aUV"       2 uvs

    uMVP            <- getShaderUniform program "uModelViewProjection"
    uInverseModel   <- getShaderUniform program "uInverseModel"
    uModel          <- getShaderUniform program "uModel"
    uCamera         <- getShaderUniform program "uCamera"

    let vertCount = fromIntegral ( length positions )

    return Geometry{..} 



