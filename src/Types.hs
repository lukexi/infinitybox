{-# LANGUAGE DeriveFunctor, DeriveTraversable, DeriveAnyClass, DeriveGeneric, FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where

import qualified Control.Monad.Trans.Free as FT
import Control.Monad.Free
import Control.Monad.Identity
import Control.Monad.Random
import Data.Binary
import GHC.Generics
import Control.Monad.Free.FromFreeT
import Control.Monad.State

import Control.Lens
import Linear
import Graphics.GL
import Data.Map (Map)

import Control.Monad.Free.Binary ()

compile :: (Traversable f, RandomGen g) => g -> FT.FreeT f (RandT g Identity) a -> Free f a
compile stdgen randInstrs = runIdentity $ evalRandT (fromFreeT randInstrs) stdgen

type ObjectID = Int

data Object = Object
    { _objPosition    :: V3 GLfloat
    , _objOrientation :: Quaternion GLfloat
    , _objScale       :: GLfloat
    } deriving (Generic, Binary, Show)

data Player = Player 
    { _plrPosition    :: V3 GLfloat
    , _plrOrientation :: Quaternion GLfloat 
    }

data World = World
    { _wldPlayer :: Player
    , _wldCubes  :: Map ObjectID Object
    , _wldLastCubes :: Map ObjectID Object
    }

makeLenses ''World
makeLenses ''Object
makeLenses ''Player

interpolateObjects :: Object -> Object -> Object
(Object p1 o1 s1) `interpolateObjects` (Object p2 o2 s2) = 
    Object (lerp 0.5 p1 p2) (slerp o1 o2 0.5) s2

newPlayer :: Player
newPlayer = Player (V3 0 5 0) (axisAngle (V3 0 1 0) 0)

newWorld :: World
newWorld = World newPlayer mempty mempty



interpret :: (MonadIO m, MonadState World m) => Free Op () -> m ()
interpret = iterM interpret'
    where
        interpret' :: (MonadIO m, MonadState World m) => Op (m t) -> m t
        interpret' (Echo s n)           = (liftIO $ putStrLn s) >> n
        interpret' (Update objID obj n) = wldCubes . at objID ?= obj >> n
        interpret' (Connect name n)     = (liftIO . putStrLn $ name ++ " connected") >> n


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


