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
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import System.Random
-- import Network.Socket (PortNumber)
-- import Data.Data
import Game.Pal
import Network.UDP.Pal
import Sound.Pd1
import Data.List

--
type ObjectID = Int
type PlayerID = String
type VoiceID = Int

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
  , _wldFrameNumber  :: !Integer
  , _wldVoiceQueue   :: ![VoiceID]
  , _wldCubeVoices   :: !(Map ObjectID VoiceID)
  , _wldVoiceOutput  :: !(Map VoiceID GLfloat)
  , _wldVoiceSources :: !(Map VoiceID OpenALSource)
  , _wldKickVoiceID  :: !VoiceID
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
  { _plrPose      = Pose (V3 0 0 1) (axisAngle (V3 0 1 0) 0)
  , _plrHeadPose  = Pose (V3 0 0 0) (axisAngle (V3 0 1 0) 0)
  , _plrHandPoses = []
  }

newWorld :: PlayerID -> Map VoiceID OpenALSource -> World
newWorld playerID sourcesByVoice = World 
  { _wldPlayer       = newPlayer
  , _wldPlayerID     = playerID 
  , _wldPlayers      = mempty 
  , _wldCubes        = mempty 
  , _wldLastCubes    = mempty 
  , _wldFrameNumber  = 0
  , _wldCubeVoices   = mempty
  , _wldVoiceQueue   = polyVoiceIDs
  , _wldVoiceOutput  = mempty 
  , _wldVoiceSources = sourcesByVoice
  , _wldKickVoiceID  = kickVoiceID
  }
  where
    allVoiceIDs = sort (Map.keys sourcesByVoice)
    (kickVoiceID:polyVoiceIDs) = allVoiceIDs

dequeueVoice :: MonadState World m => m VoiceID
dequeueVoice = do
  (x:xs) <- use wldVoiceQueue
  wldVoiceQueue .== xs ++ [x]
  return x

-- | Interpret a command into the client's world state.
-- We use a pattern here of Reliable Create/Destroy messages and Unreliable Update
-- messages. We only create and destroy the object when receiving those messages,
-- and only perform updates if the object already exists, since unreliable messages
-- can come in before the object is created and after it is destroyed.

interpret :: (MonadIO m, MonadState World m) => Op -> m ()
interpret (CreateObject objID obj)       = do
  voiceID <- dequeueVoice
  wldCubes      . at objID ?== obj
  wldCubeVoices . at objID ?== voiceID
interpret (DeleteObject objID)           = wldCubes   . at objID               .== Nothing
interpret (UpdateObject objID obj)       = wldCubes   . at objID    . traverse .== obj
interpret (UpdatePlayer playerID player) = wldPlayers . at playerID . traverse .== player
interpret (Connect playerID)             = do
  wldPlayers . at playerID ?== newPlayer
  putStrLnIO (playerID ++ " connected")
interpret (Disconnect playerID)          = do
  wldPlayers . at playerID .== Nothing
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



serverPort :: PortNumber
serverPort = 3000

packetSize :: Int
packetSize = 4096

randomName :: IO String
randomName = concat <$> replicateM 3 randomPair
  where
    randomPair = (\(x,y) -> [x,y]) . (pairs !!) <$> randomRIO (0, length pairs - 1)
    pairs = zip "bcdfghjklmnpqrstvwxz" (cycle "aeiouy")

randomColor :: MonadIO m => m (V4 GLfloat)
randomColor = liftIO $ V4 <$> randomRIO (0, 1) <*> randomRIO (0, 1) <*> randomRIO (0, 1) <*> pure 1


totalHeadPose :: Player -> Pose
totalHeadPose player = 
  let Pose playerPosit playerOrient = player ^. plrPose
      Pose headPosit headOrient     = player ^. plrHeadPose
  in  Pose 
    (headPosit  + playerPosit) 
    (headOrient * playerOrient) -- quat rotation order must be rotation*original

transformationFromPose :: Pose -> M44 GLfloat
transformationFromPose (Pose position orientation) = mkTransformation orientation position 

shiftBy :: V3 GLfloat -> Pose -> Pose
shiftBy vec pose = pose & posPosition +~ rotate (pose ^. posOrientation) vec



