{-# LANGUAGE DeriveFunctor, DeriveTraversable, DeriveAnyClass, DeriveGeneric, FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Instructions where

import qualified Control.Monad.Trans.Free as FT
import Control.Monad.Free
import Control.Monad.Identity
import Control.Monad.Random
import Data.Binary
import GHC.Generics
import Control.Monad.Free.FromFreeT

import Control.Lens
import Linear
import Graphics.GL
import Data.Map (Map)

compile :: (Traversable f, RandomGen g) => g -> FT.FreeT f (RandT g Identity) a -> Free f a
compile stdgen randInstrs = runIdentity $ evalRandT (fromFreeT randInstrs) stdgen

type ObjectID = Int

data Object = Object
    { _objPosition    :: V3 GLfloat
    , _objOrientation :: Quaternion GLfloat
    } deriving (Generic, Binary, Show)
data Player = Player 
    { _plrPosition    :: V3 GLfloat
    , _plrOrientation :: Quaternion GLfloat 
    }
data World = World
    { _wldPlayer :: Player
    , _wldCubes  :: Map ObjectID Object
    }

makeLenses ''World
makeLenses ''Object
makeLenses ''Player



-- | Deriving Generics

data Op next = Echo String next
             | Update ObjectID Object next
             | Connect String next
    deriving (Functor, Foldable, Traversable, Generic, Binary, Show)

type Instructions = Free Op ()

echo :: (Monad m) => String -> FT.FreeT Op m ()
echo b = liftF (Echo b ())

update :: (Monad m) => ObjectID -> Object -> FT.FreeT Op m ()
update a b = liftF (Update a b ())

connect :: (Monad m) => String -> FT.FreeT Op m ()
connect n = liftF (Connect n ())