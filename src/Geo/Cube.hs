module Geo.Cube where

import Graphics.GL
import Foreign
import Linear
import Graphics.GL.Pal
import Control.Monad.Trans

data Cube = Cube
        { cubeVAO                 :: VertexArrayObject
        , cubeShader              :: GLProgram
        , cubeVertexCount         :: GLsizei
        , cubeUniformMVP          :: UniformLocation
        , cubeUniformInverseModel :: UniformLocation
        , cubeUniformModel        :: UniformLocation
        , cubeUniformCamera       :: UniformLocation
        }

----------------------------------------------------------
-- Make Cube
----------------------------------------------------------

renderCube :: MonadIO m => Cube -> M44 GLfloat -> m ()
renderCube cube mvp = liftIO $ do

    useProgram (cubeShader cube)
    
    uniformM44 (cubeUniformMVP cube) mvp

    glBindVertexArray (unVertexArrayObject (cubeVAO cube))

    glDrawArrays GL_TRIANGLES 0 (cubeVertexCount cube)

    glBindVertexArray 0


makeCube :: GLProgram -> IO Cube
makeCube program = do

    aPosition       <- getShaderAttribute program "aPosition"
    aNormal         <- getShaderAttribute program "aNormal"
    aUV             <- getShaderAttribute program "aUV"
    aTangent        <- getShaderAttribute program "aTangent"
    uMVP            <- getShaderUniform   program "uModelViewProjection"
    uInverseModel   <- getShaderUniform   program "uInverseModel"
    uModel          <- getShaderUniform   program "uModel"
    uCamera         <- getShaderUniform   program "uCamera"

    -- Setup a VAO
    vaoCube <- overPtr (glGenVertexArrays 1)

    glBindVertexArray vaoCube


    -----------------
    -- Cube Positions
    -----------------
    
    -- Buffer the cube vertices
    let cubePositions = 


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
    let cubeNormals = 


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
    let cubeTangents = 

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
    let cubeUVs = 

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


    ------------------------
    -- Binding Positions
    ------------------------

    vaoCubePositions <- overPtr (glGenBuffers 1)

    glBindBuffer GL_ARRAY_BUFFER vaoCubePositions
    
    let cubePositionsSize = fromIntegral (sizeOf (undefined :: GLfloat) * length cubePositions)

    withArray cubePositions $ 
        \cubePositionsPtr ->
            glBufferData GL_ARRAY_BUFFER cubePositionsSize (castPtr cubePositionsPtr) GL_STATIC_DRAW 

    -- Describe our vertices array to OpenGL
    glEnableVertexAttribArray (fromIntegral (unAttributeLocation aPosition))

    glVertexAttribPointer
        (fromIntegral (unAttributeLocation aPosition)) -- attribute
        3                 -- number of elements per vertex, here (x,y,z)
        GL_FLOAT          -- the type of each element
        GL_FALSE          -- don't normalize
        0                 -- no extra data between each position
        nullPtr           -- offset of first element




    ------------------------
    -- Binding Normals
    ------------------------

    vaoCubeNormals <- overPtr (glGenBuffers 1)

    glBindBuffer GL_ARRAY_BUFFER vaoCubeNormals
    
    let cubeNormalsSize = fromIntegral (sizeOf (undefined :: GLfloat) * length cubeNormals)

    withArray cubeNormals $ 
        \cubeNormalsPtr ->
            glBufferData GL_ARRAY_BUFFER cubeNormalsSize (castPtr cubeNormalsPtr) GL_STATIC_DRAW 

    -- Describe our vertices array to OpenGL
    glEnableVertexAttribArray (fromIntegral (unAttributeLocation aNormal))

    glVertexAttribPointer
        (fromIntegral (unAttributeLocation aNormal)) -- attribute
        3                 -- number of elements per vertex, here (x,y,z)
        GL_FLOAT          -- the type of each element
        GL_FALSE          -- don't normalize
        0                 -- no extra data between each position
        nullPtr           -- offset of first element




    ------------------------
    -- Binding Tangents
    ------------------------

    vaoCubeTangents <- overPtr (glGenBuffers 1)

    glBindBuffer GL_ARRAY_BUFFER vaoCubeTangents
    
    let cubeTangentsSize = fromIntegral (sizeOf (undefined :: GLfloat) * length cubeTangents)

    withArray cubeTangents $ 
        \cubeTangentsPtr ->
            glBufferData GL_ARRAY_BUFFER cubeTangentsSize (castPtr cubeTangentsPtr) GL_STATIC_DRAW 

    -- Describe our vertices array to OpenGL
    glEnableVertexAttribArray (fromIntegral (unAttributeLocation aTangent))

    glVertexAttribPointer
        (fromIntegral (unAttributeLocation aTangent)) -- attribute
        3                 -- number of elements per vertex, here (x,y,z)
        GL_FLOAT          -- the type of each element
        GL_FALSE          -- don't normalize
        0                 -- no extra data between each position
        nullPtr           -- offset of first element




    ------------------------
    -- Binding UVS
    ------------------------
    
    vaoCubeUVs <- overPtr (glGenBuffers 1)

    glBindBuffer GL_ARRAY_BUFFER vaoCubeUVs
    
    let cubeUVsSize = fromIntegral (sizeOf (undefined :: GLfloat) * length cubeUVs)

    withArray cubeUVs $ 
        \cubeUVsPtr ->
            glBufferData GL_ARRAY_BUFFER cubeUVsSize (castPtr cubeUVsPtr) GL_STATIC_DRAW 

    -- Describe our vertices array to OpenGL
    glEnableVertexAttribArray (fromIntegral (unAttributeLocation aUV))

    glVertexAttribPointer
        (fromIntegral (unAttributeLocation aUV)) -- attribute
        2                 -- number of elements per vertex, here (x,y,z)
        GL_FLOAT          -- the type of each element
        GL_FALSE          -- don't normalize
        0                 -- no extra data between each position
        nullPtr           -- offset of first element


    glBindVertexArray 0

    return $ Cube 
        { cubeVAO                 = VertexArrayObject vaoCube
        , cubeShader              = program
        , cubeVertexCount         = fromIntegral (length cubePositions)
        , cubeUniformMVP          = uMVP
        , cubeUniformInverseModel = uInverseModel
        , cubeUniformModel        = uModel
        , cubeUniformCamera       = uCamera
        } 


