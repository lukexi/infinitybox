{-# OPTIONS_GHC -F -pgmF strip-ths #-}

{-# LANGUAGE DeriveFunctor, DeriveTraversable, DeriveAnyClass, DeriveGeneric, FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Types where

import Data.Binary
import GHC.Generics
import Control.Monad.State.Strict

import Control.Lens
import Linear
import Graphics.GL
import Graphics.GL.Pal2
import Data.Map.Strict (Map)
import System.Random
import Network.Socket (PortNumber)
import Data.Data
import Game.Pal
--
type ObjectID = Int
type PlayerID = String

data Object = Object
  { _objPose  :: !Pose
  , _objScale :: !GLfloat
  } deriving (Generic, Binary, Show)

data Player = Player 
  { _plrPose      :: !Pose    
  , _plrHeadPose  :: !Pose
  , _plrHandPoses :: ![Pose]
  } deriving (Generic, Binary, Show)

data World = World
  { _wldPlayer       :: !Player
  , _wldPlayerID     :: !PlayerID
  , _wldPlayers      :: !(Map PlayerID Player)
  , _wldCubes        :: !(Map ObjectID Object)
  , _wldLastCubes    :: !(Map ObjectID Object)
  , _wldPatchOutput  :: !(Map ObjectID GLfloat)
  , _wldEyeDebug     :: !(V3 GLfloat)
  , _wldFrameNumber  :: !Integer
  }

makeLenses ''Object
makeLenses ''Player
makeLenses ''World

interpolateObjects :: Object -> Object -> Object
(Object p1 s1) `interpolateObjects` (Object p2 s2) = 
  Object (interpolatePoses p1 p2) (s1 + (s2 - s1) / 2)

interpolatePoses :: Pose -> Pose -> Pose
interpolatePoses (Pose p1 o1) (Pose p2 o2) =
  Pose (lerp 0.5 p1 p2) (slerp o1 o2 0.5)

newPlayer :: Player
newPlayer = Player
  { _plrPose      = Pose (V3 0 0 0) (axisAngle (V3 0 1 0) 0)
  , _plrHeadPose  = Pose (V3 0 0 0) (axisAngle (V3 0 1 0) 0)
  , _plrHandPoses = []
  }

newWorld :: PlayerID -> World
newWorld playerID = World newPlayer playerID mempty mempty mempty mempty 0 0


interpret :: (MonadIO m, MonadState World m) => Op -> m ()
interpret (CreateObject objID obj)       = wldCubes   . at objID            ?= obj
-- traverse, here, ensures that the object is only set if it already exists
interpret (UpdateObject objID obj)       = wldCubes   . at objID . traverse .= obj
interpret (DeleteObject objID)           = wldCubes   . at objID    .= Nothing
interpret (UpdatePlayer playerID player) = wldPlayers . at playerID ?= player
interpret (Connect playerID)             = putStrLnIO (playerID ++ " connected")
interpret (Disconnect playerID)          = do
  wldPlayers . at playerID .= Nothing
  putStrLnIO (playerID ++ " disconnected")

-- | Deriving Generics

data Op = CreateObject !ObjectID !Object
        | UpdateObject !ObjectID !Object
        | DeleteObject !ObjectID
        | Connect      !PlayerID
        | UpdatePlayer !PlayerID !Player
        | Disconnect   !PlayerID
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
  , uParameter1          :: UniformLocation GLfloat
  , uParameter2          :: UniformLocation GLfloat
  , uParameter3          :: UniformLocation GLfloat
  , uParameter4          :: UniformLocation GLfloat
  , uParameter5          :: UniformLocation GLfloat
  , uParameter6          :: UniformLocation GLfloat
  , uID                  :: UniformLocation GLfloat
  , uTime                :: UniformLocation GLfloat
  } deriving (Data)

serverPort :: PortNumber
serverPort = 3000

serverName :: String
serverName = "127.0.0.1"
-- serverName = "10.0.1.158"

packetSize :: Int
packetSize = 4096

randomName :: IO String
randomName = concat <$> replicateM 3 randomPair
  where
    randomPair = (\(x,y) -> [x,y]) . (pairs !!) <$> randomRIO (0, length pairs - 1)
    pairs = zip "bcdfghjklmnpqrstvwxz" (cycle "aeiouy")

randomColor :: MonadIO m => m (V4 GLfloat)
randomColor = liftIO $ V4 <$> randomRIO (0, 1) <*> randomRIO (0, 1) <*> randomRIO (0, 1) <*> pure 1


totalHeadPose :: (MonadState World m) => m Pose
totalHeadPose = do
  Pose playerPosit playerOrient <- use (wldPlayer . plrPose)
  Pose headPosit headOrient     <- use (wldPlayer . plrHeadPose)
  return $ Pose 
    (headPosit  + playerPosit) 
    (headOrient * playerOrient) -- quat rotation order must be rotation*original