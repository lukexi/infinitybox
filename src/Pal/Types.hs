module Pal.Types where

import Graphics.GL
import Control.Monad.Trans
import Foreign
import Linear


newtype Program             = Program             { unProgram             :: GLuint }

newtype AttributeLocation   = AttributeLocation   { unAttributeLocation   :: GLint  }
newtype UniformLocation     = UniformLocation     { unUniformLocation     :: GLint  }
newtype TextureID           = TextureID           { unTextureID           :: GLuint }

newtype VertexArrayObject   = VertexArrayObject   { unVertexArrayObject   :: GLuint }
newtype ArrayBuffer         = ArrayBuffer         { unArrayBuffer         :: GLuint }
newtype ElementArrayBuffer  = ElementArrayBuffer  { unElementArrayBuffer  :: GLuint }

newtype TextureObject       = TextureObject       { unTextureObject       :: GLuint }

data ColorSpace = SRGB | Linear


data Uniforms = Uniforms

  { uMVP          :: UniformLocation
  , uInverseModel :: UniformLocation
  , uModel        :: UniformLocation
  , uCamera       :: UniformLocation
  } 


data Geometry = Geometry

  { positions     :: !ArrayBuffer
  , normals       :: !ArrayBuffer
  , tangents      :: !ArrayBuffer
  , uvs           :: !ArrayBuffer
  , indices       :: !ElementArrayBuffer
  , vertCount     :: !GLsizei
  }

data Shape = Shape

  { positionList  :: ![ GLfloat ]
  , normalList    :: ![ GLfloat ]
  , tangentList   :: ![ GLfloat ]
  , uvList        :: ![ GLfloat ]
  , indexList     :: ![ GLuint  ]
  , numVerts      :: !GLsizei
  , numPoints     :: !GLuint
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

  { program   :: !Program
  , uniforms  :: !Uniforms
  , geometry  :: !Geometry
  , vAO       :: !VertexArrayObject
  }


-- | Utility for extracting a value from a pointer-taking function
overPtr :: (MonadIO m, Storable a) => (Ptr a -> IO b) -> m a
overPtr f = liftIO (alloca (\p -> f p >> peek p))
