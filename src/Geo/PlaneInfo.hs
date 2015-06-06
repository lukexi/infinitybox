{-# LANGUAGE RecordWildCards #-}

module Geo.PlaneInfo where

import Graphics.GL
import Graphics.GL.Pal
import Geo.AssignAttribute
import GELP.WithActions
import Geo.Geometry


initPlaneGeometry :: GLProgram -> IO Geometry
initPlaneGeometry program = do

    -----------------
    -- Plane Positions
    -----------------
    
    -- Buffer the plane vertices
    let positions = map (*100)


            -- front
            [ -1.0 ,  1.0 ,  0.0
            , -1.0 , -1.0 ,  0.0 
            ,  1.0 ,  1.0 ,  0.0

            ,  1.0 , -1.0 ,  0.0
            ,  1.0 ,  1.0 ,  0.0
            , -1.0 , -1.0 ,  0.0

            ] :: [GLfloat]




    -----------------
    -- Plane Normals
    -----------------

    -- Buffer the plane vertices
    let normals = take 18 $ cycle [0,0,1] :: [GLfloat]


    -----------------
    -- Plane Tangents
    -----------------

    -- Buffer the plane vertices
    let tangents = take 18 $ cycle [1,0,0] :: [GLfloat]



    -----------------
    -- Plane UVs
    -----------------

    -- Buffer the plane vertices
    let uvs = 

            -- front
            [  0.0 ,  1.0
            ,  0.0 ,  0.0
            ,  1.0 ,  1.0

            ,  1.0 ,  0.0 
            ,  1.0 ,  1.0 
            ,  0.0 ,  0.0 

            ] :: [GLfloat]


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

