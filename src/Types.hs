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
import System.Hardware.Hydra
import Game.Pal
import Network.UDP.Pal
import Sound.Pd1
import Data.List
import Graphics.GL.Pal
import Data.Data
import Animation.Pal
import Data.Time
import Control.Monad.Random
--

-- | The maximum number of cubes before we start kicking cubes out
maxCubes :: Int
maxCubes = 16

-- FIXME: We're using Word32 because we can't pass anything 
-- bigger as a RigidBodyID or else it will be silently truncated.
-- Using the RigidBody userPointer might be the safer bet,
-- or make RigidBodyID a Word32 too so Haskell will track type safety
type ObjectID = Word32 
type PlayerID = String
type VoiceID = Int

data Visuals = Visuals
  { _cubeShader  :: !FilePath
  , _logoShader  :: !FilePath
  , _roomShader  :: !FilePath
  , _handShader  :: !FilePath
  , _faceShader  :: !FilePath
  , _lightShader :: !FilePath
  , _vertShader  :: !FilePath
  }

data Uniforms = Uniforms
  { uModelViewProjection :: UniformLocation (M44 GLfloat)
  , uInverseModel        :: UniformLocation (M44 GLfloat)
  , uModel               :: UniformLocation (M44 GLfloat)
  , uCamera              :: UniformLocation (V3  GLfloat)
  , uLight1              :: UniformLocation (V3  GLfloat)
  , uLight2              :: UniformLocation (V3  GLfloat)
  , uLight3              :: UniformLocation (V3  GLfloat)
  , uLight4              :: UniformLocation (V3  GLfloat)
  , uFilledness          :: UniformLocation GLfloat
  , uComplete            :: UniformLocation GLfloat
  , uParameter1          :: UniformLocation GLfloat
  , uParameter2          :: UniformLocation GLfloat
  , uParameter3          :: UniformLocation GLfloat
  , uParameter4          :: UniformLocation GLfloat
  , uParameter5          :: UniformLocation GLfloat
  , uParameter6          :: UniformLocation GLfloat
  , uID                  :: UniformLocation GLfloat
  , uTime                :: UniformLocation GLfloat
  , uTick                :: UniformLocation GLfloat
  , uStarted             :: UniformLocation GLfloat
  } deriving (Data)

data Resources = Resources
  { room  :: Shape Uniforms
  , cube  :: Shape Uniforms
  , light :: Shape Uniforms
  , hand  :: Shape Uniforms
  , face  :: Shape Uniforms
  , logo  :: Shape Uniforms
  }


data Themes = Themes
  { _rainbow :: Resources
  , _ao      :: Resources
  }

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
  , _wldCubeAges     :: !(Map ObjectID Float)
  , _wldHandTriggers :: !(Map WhichHand Bool) -- ^ Lets us detect new trigger pushes
  , _wldFilledness   :: !(Animation Float)
  , _wldComplete     :: !(Animation Float)
  , _wldPhase        :: !Phase
  , _wldTime         :: !Float
  , _wldStarted      :: !Float
  }

data Phase = PhaseVoid | PhaseLogo | PhaseMain | PhaseEnd deriving Eq

makeLenses ''Object
makeLenses ''Player
makeLenses ''World
makeLenses ''Visuals
makeLenses ''Themes

interpolateObjects :: Object -> Object -> Object
(Object p1 s1) `interpolateObjects` (Object p2 s2) = 
  Object (interpolatePoses p1 p2) (s1 + (s2 - s1) / 2)

interpolatePoses :: Pose -> Pose -> Pose
interpolatePoses (Pose p1 o1) (Pose p2 o2) =
  Pose (lerp 0.5 p1 p2) (slerp o1 o2 0.5)

newPlayer1 :: Player
newPlayer1 = Player
  { _plrPose      = Pose (V3 0 (-3) 5.6) (axisAngle (V3 0 1 0) 0)
  , _plrHeadPose  = Pose (V3 0 0 0) (axisAngle (V3 0 1 0) 0)
  , _plrHandPoses = []
  }

newPlayer2 :: Player
newPlayer2 = Player
  { _plrPose      = Pose (V3 0 (-3) (-5.6)) (axisAngle (V3 0 1 0) pi)
  , _plrHeadPose  = Pose (V3 0 0 0) (axisAngle (V3 0 1 0) 0)
  , _plrHandPoses = []
  }

newWorld :: PlayerID -> Player -> Map VoiceID OpenALSource -> DiffTime -> World
newWorld playerID player sourcesByVoice now = World 
  { _wldPlayer       = player
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
  , _wldCubeAges     = mempty
  , _wldHandTriggers = mempty  
  , _wldFilledness   = Animation
      { animStart    = now
      , animDuration = 1
      , animFunc     = anim id
      , animFrom     = 0
      , animTo       = 0
      }
  , _wldComplete     = Animation
      { animStart    = now
      , animDuration = 1
      , animFunc     = anim id
      , animFrom     = 0
      , animTo       = 0
      }
  , _wldPhase        = PhaseVoid
  , _wldTime         = 0
  , _wldStarted      = 0
  }

  where
    allVoiceIDs = sort (Map.keys sourcesByVoice)
    (kickVoiceID:polyVoiceIDs) = if null allVoiceIDs then [0] else allVoiceIDs

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
  wldCubeAges   . at objID ?== 0
  
  -- wldFilledness += 1.0 / fromIntegral maxCubes 

  fillednessAnim  <- use wldFilledness
  -- completeAnim    <- use wldComplete

  numCubes <- Map.size <$> use wldCubes
  now <- getNow
  wldFilledness .= Animation
        { animStart = now
        , animDuration = 1
        , animFunc = anim id
        , animFrom = evanResult (evalAnim now fillednessAnim)
        , animTo = (1 / fromIntegral maxCubes) * fromIntegral numCubes
        }


interpret (DeleteObject objID)           = do
  wldCubes      . at objID .== Nothing
  wldCubeAges   . at objID .== Nothing
  wldCubeVoices . at objID .== Nothing

interpret (UpdateObject objID obj)       = 
  wldCubes   . at objID    . traverse .== obj

interpret (UpdatePlayer playerID player) = 
  wldPlayers . at playerID . traverse .== player

interpret (Connect playerID player)      = do
  wldPlayers . at playerID ?== player
  putStrLnIO (playerID ++ " connected")
  
interpret (Disconnect playerID)          = do
  wldPlayers . at playerID .== Nothing
  putStrLnIO (playerID ++ " disconnected")

interpret (ObjectCollision objectAID objectBID strength) = do
  -- putStrLnIO $ "Client got collision! " ++ show objectAID ++ " " ++ show objectBID ++ ": " ++ show strength
  forM_ [objectAID, objectBID] $ \objID -> do
    mVoiceID <- use (wldCubeVoices . at objID)
    -- The objectID may be invalid since we send hands and walls as objectIDs,
    -- thus we may not have a voice for them.
    let volume = min 1 (strength * 5)
    
    forM_ mVoiceID $ \voiceID -> do
      wldVoiceOutput . at voiceID ?== volume
      liftIO $ sendGlobal (show voiceID ++ "trigger") $ 
        Atom (Float volume)


-- | Deriving Generics

data Op = CreateObject    !ObjectID !Object
        | UpdateObject    !ObjectID !Object
        | DeleteObject    !ObjectID
        | Connect         !PlayerID !Player
        | UpdatePlayer    !PlayerID !Player
        | Disconnect      !PlayerID
        | ObjectCollision !ObjectID !ObjectID !Float
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
totalHeadPose player = addPoses (player ^. plrPose) (player ^. plrHeadPose)


newCubeInstruction :: MonadRandom m => Pose -> m Op
newCubeInstruction pose = do
  objID <- getRandom
  return $ CreateObject objID (Object pose cubeScale)

-- Offset the lights to be on the end of the wands
handLightOffset :: V3 GLfloat
handLightOffset = handOffset * 2

-- Offset the hand model outward to feel like wands rather than batons
handOffset :: V3 GLfloat
handOffset = V3 0 0 (-(handDimensions ^. _z) / 2)

handDimensions :: V3 GLfloat
handDimensions = V3 0.05 0.05 0.5


cubeScale :: GLfloat
cubeScale = 0.4

roomScale :: GLfloat
roomScale = 10

