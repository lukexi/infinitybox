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
import Control.Monad.Free.Binary ()
import Control.Monad.State

import Control.Lens
import Linear
import Graphics.GL
import Data.Map (Map)

type ObjectID = Int
type PlayerID = Int

data Pose = Pose
  { _posPosition    :: V3 GLfloat
  , _posOrientation :: Quaternion GLfloat 
  } deriving (Generic, Binary, Show)

data Object = Object
  { _objPosition    :: V3 GLfloat
  , _objOrientation :: Quaternion GLfloat
  , _objScale       :: GLfloat
  } deriving (Generic, Binary, Show)

data Player = Player 
  { _plrPose :: Pose    
  , _plrHandPoses :: [Pose]
  } deriving (Generic, Binary, Show)

data World = World
  { _wldPlayer       :: Player
  , _wldPlayerID     :: PlayerID
  , _wldPlayers      :: Map PlayerID Player
  , _wldCubes        :: Map ObjectID Object
  , _wldLastCubes    :: Map ObjectID Object
  , _wldEyeDebug     :: V3 GLfloat
  , _wldFrameNumber  :: Integer
  }

makeLenses ''Pose
makeLenses ''Object
makeLenses ''Player
makeLenses ''World

interpolateObjects :: Object -> Object -> Object
(Object p1 o1 s1) `interpolateObjects` (Object p2 o2 s2) = 
  Object (lerp 0.5 p1 p2) (slerp o1 o2 0.5) (s1 + (s2 - s1) / 2)

newPlayer :: Player
newPlayer = Player (Pose (V3 0 5 0) (axisAngle (V3 0 1 0) 0)) []

newWorld :: PlayerID -> World
newWorld playerID = World newPlayer playerID mempty mempty mempty 0 0

interpret :: (MonadIO m, MonadState World m) => Free Op () -> m ()
interpret = iterM interpret'
  where
    interpret' :: (MonadIO m, MonadState World m) => Op (m t) -> m t
    interpret' (UpdateObject objID obj n)       = wldCubes   . at objID    ?= obj    >> n
    interpret' (UpdatePlayer playerID player n) = wldPlayers . at playerID ?= player >> n
    interpret' (Connect name n)                 = (liftIO . putStrLn $ name ++ " connected") >> n


-- Get a regular Free value out of a FreeT
compile :: (Traversable f, RandomGen g) => g -> FT.FreeT f (RandT g Identity) a -> Free f a
compile stdgen randInstrs = runIdentity $ evalRandT (fromFreeT randInstrs) stdgen

-- | Deriving Generics

data Op next = UpdateObject ObjectID Object next
             | UpdatePlayer PlayerID Player next
             | Connect      String next
  deriving (Functor, Foldable, Traversable, Generic, Binary, Show)

type Instructions = Free Op ()

updateObject :: (Monad m) => ObjectID -> Object -> FT.FreeT Op m ()
updateObject a b = liftF (UpdateObject a b ())

updatePlayer :: (Monad m) => PlayerID -> Player -> FT.FreeT Op m ()
updatePlayer a b = liftF (UpdatePlayer a b ())

connect :: (Monad m) => String -> FT.FreeT Op m ()
connect n = liftF (Connect n ())


