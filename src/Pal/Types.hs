module Pal.Types where

import Graphics.GL
import Control.Monad.Trans
import Foreign

newtype GLProgram         = GLProgram           { unGLProgram           :: GLuint }

newtype AttributeLocation = AttributeLocation   { unAttributeLocation   :: GLint  }
newtype UniformLocation   = UniformLocation     { unUniformLocation     :: GLint  }
newtype TextureID         = TextureID           { unTextureID           :: GLuint }

newtype VertexArrayObject = VertexArrayObject   { unVertexArrayObject   :: GLuint }
newtype ArrayBuffer       = ArrayBuffer         { unArrayBuffer         :: GLuint }

newtype TextureObject     = TextureObject       { unTextureObject       :: GLuint }

data ColorSpace = SRGB | Linear

data Uniforms = Uniforms

  { uMVP          :: UniformLocation
  , uInverseModel :: UniformLocation
  , uModel        :: UniformLocation
  , uCamera       :: UniformLocation
  } 


data Geometry = Geometry

  { positions     :: ArrayBuffer
  , normals       :: ArrayBuffer
  , tangents      :: ArrayBuffer
  , uvs           :: ArrayBuffer
  , vertCount     :: GLsizei
  }

{-
data Body = Body

  { position  :: V3 Float
  , rotation  :: Quaternion
  , scale     :: V3 Float
  , rigidBody :: RigidBody
  }
-}

data Entity = Entity

  { program   :: GLProgram
  , uniforms  :: Uniforms
  , geometry  :: Geometry
  , vAO       :: VertexArrayObject
  }

-- | Utility for extracting a value from a pointer-taking function
overPtr :: (MonadIO m, Storable a) => (Ptr a -> IO b) -> m a
overPtr f = liftIO (alloca (\p -> f p >> peek p))
