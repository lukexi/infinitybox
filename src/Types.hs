{-# LANGUAGE DeriveFunctor, DeriveTraversable, DeriveAnyClass, DeriveGeneric, FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Types where

import Data.Binary
import GHC.Generics
import Control.Monad.State

import Control.Lens
import Linear
import Graphics.GL
import Graphics.GL.Pal2
import Data.Map (Map)
import System.Random
import Network.Socket (PortNumber)
import Data.Data

type ObjectID = Int
type PlayerID = String

data Object = Object
  { _objPose  :: Pose
  , _objScale :: GLfloat
  } deriving (Generic, Binary, Show)

data Player = Player 
  { _plrPose :: Pose    
  , _plrHandPoses :: [Pose]
  , _plrHeadPose :: Pose
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
newPlayer = Player
  { _plrPose = Pose (V3 0 5 0) (axisAngle (V3 0 1 0) 0)
  , _plrHandPoses = []
  , _plrHeadPose = Pose (V3 0 0 0) (axisAngle (V3 0 1 0) 0)
  }

newWorld :: PlayerID -> World
newWorld playerID = World newPlayer playerID mempty mempty mempty 0 0


interpret :: (MonadIO m, MonadState World m) => Op -> m ()
interpret (UpdateObject objID obj)       = wldCubes   . at objID    ?= obj
interpret (UpdatePlayer playerID player) = wldPlayers . at playerID ?= player
interpret (Connect name)                 = putStrLnIO (name ++ " connected")
interpret (Disconnect name)              = putStrLnIO (name ++ " disconnected")

-- | Deriving Generics

data Op = UpdateObject ObjectID Object
        | UpdatePlayer PlayerID Player
        | Connect      PlayerID
        | Disconnect   PlayerID
  deriving (Generic, Binary, Show)

-- Util

putStrLnIO :: MonadIO m => String -> m ()
putStrLnIO = liftIO . putStrLn

printIO :: (Show s, MonadIO m) => s -> m ()
printIO = putStrLnIO . show

data Uniforms = Uniforms
  { uModelViewProjection :: UniformLocation (M44 GLfloat)
  , uInverseModel        :: UniformLocation (M44 GLfloat)
  , uModel               :: UniformLocation (M44 GLfloat)
  , uCamera              :: UniformLocation (V3  GLfloat)
  , uLight1              :: UniformLocation (V3  GLfloat)
  , uLight2              :: UniformLocation (V3  GLfloat)
  , uLight3              :: UniformLocation (V3  GLfloat)
  , uLight4              :: UniformLocation (V3  GLfloat)
  , uID                  :: UniformLocation GLfloat
  } deriving (Data)

serverPort :: PortNumber
serverPort = 3000

serverName :: String
serverName = "127.0.0.1"

packetSize :: Int
packetSize = 4096

randomName :: IO String
randomName = concat <$> replicateM 3 randomPair
  where
    randomPair = (\(x,y) -> [x,y]) . (pairs !!) <$> randomRIO (0, length pairs - 1)
    pairs = zip "bcdfghjklmnpqrstvwxz" (cycle "aeiouy")

randomColor :: MonadIO m => m (V4 GLfloat)
randomColor = liftIO $ V4 <$> randomRIO (0, 1) <*> randomRIO (0, 1) <*> randomRIO (0, 1) <*> pure 1


