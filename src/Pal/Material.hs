module Pal.Material where
import Graphics.GL
import Graphics.GL.Pal

data Material = Material
        { vAO           :: VertexArrayObject
        , program       :: GLProgram
        , vertCount     :: GLsizei
        , uMVP          :: UniformLocation
        , uInverseModel :: UniformLocation
        , uModel        :: UniformLocation
        , uCamera       :: UniformLocation
        }