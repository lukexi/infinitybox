{-# LANGUAGE DeriveFunctor, DeriveTraversable, DeriveAnyClass, DeriveGeneric, FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Types where

import Data.Binary
import GHC.Generics
import Control.Monad.State.Strict

import Control.Lens.Extra
import Linear.Extra
import Graphics.GL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import System.Random
import Network.UDP.Pal
import Sound.Pd
import Data.List
import Graphics.GL.Pal
import Animation.Pal
import Data.Time
import Control.Monad.Random
import Physics.Bullet

import Graphics.VR.Pal

data ServerIPType = UseLocalhost | UsePublicIP


data Op = CreateObject    !ObjectID !Object
        | UpdateObject    !ObjectID !Object
        | DeleteObject    !ObjectID
        | Connect         !PlayerID !Player
        | UpdatePlayer    !PlayerID !Player
        | Disconnect      !PlayerID
        | ObjectCollision !Collision
        | Restart
  deriving (Generic, Binary, Show)


-- | The maximum number of cubes before we start kicking cubes out
maxCubes :: Int
maxCubes = 15

dayLength :: Float
dayLength = 120

dayNightCycleAt :: Float -> Float
dayNightCycleAt t = dayNightCycle
  where 
    speedTime = t / dayLength;
    dayNightCycle = sin (speedTime * 2 * pi)

-- FIXME: We're using Word32 because we can't pass anything 
-- bigger as a RigidBodyID or else it will be silently truncated.
-- Using the RigidBody userPointer might be the safer bet,
-- or make RigidBodyID a Word32 too so Haskell will track type safety
type ObjectID = Word32 
type PlayerID = String
type VoiceID = Int

data Visuals = Visuals
  { _cubeShader     :: !FilePath
  , _logoShader     :: !FilePath
  , _roomShader     :: !FilePath
  , _handShader     :: !FilePath
  , _handleShader   :: !FilePath
  , _faceShader     :: !FilePath
  , _lightShader    :: !FilePath
  , _vertShader     :: !FilePath
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
  , uCollisionPosition   :: UniformLocation (V3  GLfloat)
  , uCollisionTime       :: UniformLocation GLfloat
  , uFilledness          :: UniformLocation GLfloat
  , uComplete            :: UniformLocation GLfloat
  , uParameterA          :: UniformLocation (V3  GLfloat)
  , uParameterB          :: UniformLocation (V3  GLfloat)
  , uID                  :: UniformLocation GLfloat
  , uTime                :: UniformLocation GLfloat
  , uTick                :: UniformLocation GLfloat
  , uStarted             :: UniformLocation GLfloat
  , uDayNight            :: UniformLocation GLfloat
  , uDayLength           :: UniformLocation GLfloat
  } deriving (Data)

data Shapes = Shapes
  { room    :: Shape Uniforms
  , cube    :: Shape Uniforms
  , light   :: Shape Uniforms
  , hand    :: Shape Uniforms
  , handle  :: Shape Uniforms
  , face    :: Shape Uniforms
  , logo    :: Shape Uniforms
  }

data Object = Object
  { _objPose  :: !(Pose GLfloat)
  , _objScale :: !GLfloat
  } deriving (Generic, Binary, Show)

data Player = Player 
  { _plrPose       :: !(Pose GLfloat)
  , _plrHeadPose   :: !(Pose GLfloat)
  , _plrHandPoses  :: ![Pose GLfloat]
  , _plrHandVacuum :: ![Bool]
  , _plrVacuum     :: !Bool
  } deriving (Generic, Binary, Show)

data CubeCollision = CubeCollision
  { _ccPosition  :: !(V3 GLfloat)
  , _ccImpulse   :: !GLfloat
  , _ccDirection :: !(V3 GLfloat)
  , _ccTime      :: !Float
  }

data World = World
  { _wldPlayer         :: !Player
  , _wldPlayerID       :: !PlayerID
  , _wldPlayers        :: !(Map PlayerID Player)
  , _wldLastPlayers    :: !(Map PlayerID Player)
  , _wldCubes          :: !(Map ObjectID Object)
  , _wldLastCubes      :: !(Map ObjectID Object)
  , _wldLastCollisions :: !(Map ObjectID CubeCollision)
  , _wldFrameNumber    :: !Integer
  , _wldVoiceQueue     :: ![VoiceID]
  , _wldCubeVoices     :: !(Map ObjectID VoiceID)
  , _wldVoicePitch     :: !(Map VoiceID GLfloat)
  , _wldVoiceAmplitude :: !(Map VoiceID GLfloat)
  , _wldVoiceSources   :: !(Map VoiceID OpenALSource)
  , _wldKickVoiceID    :: !VoiceID
  , _wldLastHands      :: !(Map HandID Hand) -- ^ Lets us detect new button pushes
  , _wldFilledness     :: !(Animation Float)
  , _wldPhase          :: !Phase
  , _wldTime           :: !Float
  , _wldStarted        :: !Float
  }

data Phase = PhaseVoid | PhaseLogo | PhaseMain | PhaseEnd deriving Eq

makeLenses ''Object
makeLenses ''Player
makeLenses ''World
makeLenses ''Visuals
makeLenses ''CubeCollision

interpolateObjects :: Object -> Object -> Object
(Object poseA scaleA) `interpolateObjects` (Object poseB scaleB) = 
  Object (interpolatePoses poseA poseB) (scaleA + (scaleB - scaleA) / 2)


interpolatePlayers  :: Player -> Player -> Player
(Player poseA headPoseA handsA _ _) `interpolatePlayers` (Player poseB headPoseB handsB handVacB vacB) = 
  Player
    { _plrPose       = interpolatePoses poseA poseB
    , _plrHeadPose   = interpolatePoses headPoseA headPoseB
    , _plrHandPoses  = zipWith interpolatePoses handsA handsB
    , _plrHandVacuum = handVacB
    , _plrVacuum     = vacB
    }

newPlayer :: RoomScale -> Player
newPlayer isRoomScale = Player
  { _plrPose      = if isRoomScale == RoomScale 
      then Pose (V3 0 ((-roomScale/2)) 0) (axisAngle (V3 0 1 0) 0)
      else Pose (V3 0 (-roomScale/3) (roomScale/3))          (axisAngle (V3 0 1 0) 0)
  , _plrHeadPose  = newPose
  , _plrHandPoses = []
  , _plrHandVacuum = []
  , _plrVacuum     = False
  }

logoObject :: Object
logoObject = Object (Pose (V3 0 (-4) (-5)) (axisAngle (V3 0 1 0) 0)) 3

newWorld :: PlayerID -> Player -> Map VoiceID OpenALSource -> DiffTime -> World
newWorld playerID player sourcesByVoice now = World 
  { _wldPlayer         = player
  , _wldPlayerID       = playerID 
  , _wldPlayers        = mempty 
  , _wldLastPlayers    = mempty
  , _wldCubes          = mempty 
  , _wldLastCubes      = mempty 
  , _wldFrameNumber    = 0
  , _wldCubeVoices     = mempty
  , _wldVoiceQueue     = polyVoiceIDs
  , _wldVoicePitch     = mempty 
  , _wldVoiceAmplitude = mempty 
  , _wldVoiceSources   = sourcesByVoice
  , _wldKickVoiceID    = kickVoiceID
  , _wldLastHands      = mempty
  , _wldFilledness     = Animation
      { animStart      = now
      , animDuration   = 1
      , animFunc       = anim id
      , animFrom       = 0
      , animTo         = 0
      }
  , _wldPhase          = PhaseVoid
  , _wldTime           = 0
  , _wldStarted        = 0
  , _wldLastCollisions = mempty
  }

  where
    allVoiceIDs = sort (Map.keys sourcesByVoice)
    (kickVoiceID:polyVoiceIDs) = if null allVoiceIDs then [0] else allVoiceIDs



dequeueVoice :: MonadState World m => m VoiceID
dequeueVoice = do
  (x:xs) <- use wldVoiceQueue
  wldVoiceQueue .= xs ++ [x]
  return x




serverPort :: PortNumber
serverPort = 54321

packetSize :: Int
packetSize = 4096

randomName :: IO String
randomName = concat <$> replicateM 3 randomPair
  where
    randomPair = (\(x,y) -> [x,y]) . (pairs !!) <$> randomRIO (0, length pairs - 1)
    pairs = zip "bcdfghjklmnpqrstvwxz" (cycle "aeiouy")

randomColor :: MonadIO m => m (V4 GLfloat)
randomColor = liftIO $ V4 <$> randomRIO (0, 1) <*> randomRIO (0, 1) <*> randomRIO (0, 1) <*> pure 1


totalHeadPose :: Player -> Pose GLfloat
totalHeadPose player = addPoses (player ^. plrPose) (player ^. plrHeadPose)


newCubeInstruction :: MonadRandom m => Pose GLfloat -> m Op
newCubeInstruction pose = do
  objID <- getRandom
  return $ CreateObject objID (Object pose initialCubeScale)

-- Offset the lights to be on the end of the wands
handLightOffset :: V3 GLfloat
handLightOffset = handOffset * 2


handDimensions :: V3 GLfloat
handDimensions = V3 0.055 0.055 0.5

handleDimensions :: V3 GLfloat
handleDimensions = V3 0.038 0.038 0.1651

handleOffset :: V3 GLfloat
handleOffset = V3 0 0 ((handleDimensions ^. _z) / 2)

-- Offset the hand model outward to feel like wands rather than batons
handOffset :: V3 GLfloat
handOffset = V3 0 0 (-(handDimensions ^. _z) / 2)

initialCubeScale :: GLfloat
initialCubeScale = 0.01

cubeScale :: GLfloat
cubeScale = 0.4

roomScale :: GLfloat
roomScale = 10

silenceVoice :: MonadIO m => OpenALSource -> m ()
silenceVoice sourceID = 
  alSourcePosition sourceID (V3 0 0 (-10000) :: V3 GLfloat)

roomRigidBodyID :: RigidBodyID
roomRigidBodyID = RigidBodyID 0

leftHandRigidBodyID :: RigidBodyID
leftHandRigidBodyID = RigidBodyID 1

rightHandRigidBodyID :: RigidBodyID
rightHandRigidBodyID = RigidBodyID 2

handRigidBodyIDs :: [RigidBodyID]
handRigidBodyIDs = [leftHandRigidBodyID, rightHandRigidBodyID]