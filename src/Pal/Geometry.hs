module Pal.Geometry where
import Graphics.GL
import Graphics.GL.Pal

data Geometry = Geometry
        { vAO           :: VertexArrayObject
        , program       :: GLProgram
        , vertCount     :: GLsizei
        , uMVP          :: UniformLocation
        , uInverseModel :: UniformLocation
        , uModel        :: UniformLocation
        , uCamera       :: UniformLocation
        }