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
import Sound.Pd1
import Data.List
import Graphics.GL.Pal
import Animation.Pal
import Data.Time
-- import Data.Maybe
import Control.Monad.Random
import Physics.Bullet

import System.CPUTime
import Text.Printf
import Graphics.VR.Pal

data ServerIPType = UseLocalhost | UsePublicIP

-- | The maximum number of cubes before we start kicking cubes out
maxCubes :: Int
maxCubes = 15

dayLength :: Float
dayLength = 30

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

data Resources = Resources
  { room    :: Shape Uniforms
  , cube    :: Shape Uniforms
  , light   :: Shape Uniforms
  , hand    :: Shape Uniforms
  , handle  :: Shape Uniforms
  , face    :: Shape Uniforms
  , logo    :: Shape Uniforms
  }


data Themes = Themes
  { _rainbow :: Resources
  , _ao      :: Resources
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
  , _wldCubeAges       :: !(Map ObjectID Float)
  , _wldLastHands      :: !(Map HandID Hand) -- ^ Lets us detect new button pushes
  , _wldFilledness     :: !(Animation Float)
  , _wldComplete       :: !(Animation Float)
  , _wldPhase          :: !Phase
  , _wldTime           :: !Float
  , _wldStarted        :: !Float
  }

data Phase = PhaseVoid | PhaseLogo | PhaseMain | PhaseEnd deriving Eq

makeLenses ''Object
makeLenses ''Player
makeLenses ''World
makeLenses ''Visuals
makeLenses ''Themes
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
  , _wldCubeAges       = mempty
  , _wldLastHands      = mempty
  , _wldFilledness     = Animation
      { animStart      = now
      , animDuration   = 1
      , animFunc       = anim id
      , animFrom       = 0
      , animTo         = 0
      }
  , _wldComplete       = Animation
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

-- | Interpret a command into the client's world state.
-- We use a pattern here of Reliable Create/Destroy messages and Unreliable Update
-- messages. We only create and destroy the object when receiving those messages,
-- and only perform updates if the object already exists, since unreliable messages
-- can come in before the object is created and after it is destroyed.

interpret :: (MonadIO m, MonadState World m) => Op -> m ()

interpret (CreateObject objID obj)       = do
  voiceID <- dequeueVoice
  
  wldCubes      . at objID ?= obj
  wldCubeVoices . at objID ?= voiceID
  wldCubeAges   . at objID ?= 0

  liftIO $ sendGlobal (show voiceID ++ "new-phrase") Bang
  
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
  mVoiceID <- use $ wldCubeVoices . at objID
  forM_ mVoiceID $ \voiceID -> do
    mSourceID <- use $ wldVoiceSources . at voiceID
    forM_ mSourceID silenceVoice
  wldCubes      . at objID .= Nothing
  wldCubeAges   . at objID .= Nothing
  wldCubeVoices . at objID .= Nothing

interpret (UpdateObject objID obj)       = 
  wldCubes   . at objID    . traverse .= obj

interpret (UpdatePlayer playerID player) = 
  wldPlayers . at playerID . traverse .= player

interpret (Connect playerID player)      = do
  wldPlayers . at playerID ?= player
  putStrLnIO (playerID ++ " connected")
  
interpret (Disconnect playerID)          = do
  wldPlayers . at playerID .= Nothing
  putStrLnIO (playerID ++ " disconnected")

interpret (ObjectCollision _collision) = do
  return ()
  -- putStrLnIO $ "Client got collision! " ++ show objectAID ++ " " ++ show objectBID ++ ": " ++ show strength
  -- now <- use wldTime
  -- let objectIDs = fromIntegral <$> sequence [cbBodyAID, cbBodyBID]                collision
  --     positions =                  sequence [cbPositionOnA, cbPositionOnB]          collision
  --     normals   =                  sequence [negate . cbNormalOnB , cbNormalOnB]  collision
  --     impulse   = cbAppliedImpulse collision
  -- forM_ (zip3 objectIDs positions normals) $ \(objID, position, normal) -> do

  --   mObj <- use $ wldCubes . at objID 
  --   case mObj of 
  --     Nothing -> return ()
  --     Just obj -> do
  --       let model = transformationFromPose (obj ^. objPose)
  --           scaledModel = model !*! scaleMatrix (realToFrac (obj ^. objScale))
  --           invModel = fromMaybe scaledModel (inv44 scaledModel) :: M44 GLfloat
  --           positionPoint = point position :: V4 GLfloat

  --       wldLastCollisions . at objID ?= CubeCollision

  --         { _ccTime = now
  --         , _ccImpulse = impulse
  --         , _ccPosition = normalizePoint (invModel !* positionPoint)
  --         , _ccDirection = normal
  --         }

  --   mVoiceID <- use (wldCubeVoices . at objID)
  --   -- The objectID may be invalid since we send hands and walls as objectIDs,
  --   -- thus we may not have a voice for them.
  --   let volume = min 1 (impulse * 5)
    
  --   forM_ mVoiceID $ \voiceID -> do
  --     wldVoicePitch . at voiceID ?= volume
  --     liftIO $ sendGlobal (show voiceID ++ "trigger") $ 
  --       Atom (Float volume)
interpret Restart = return ()


-- | Deriving Generics

data Op = CreateObject    !ObjectID !Object
        | UpdateObject    !ObjectID !Object
        | DeleteObject    !ObjectID
        | Connect         !PlayerID !Player
        | UpdatePlayer    !PlayerID !Player
        | Disconnect      !PlayerID
        | ObjectCollision !Collision
        | Restart
  deriving (Generic, Binary, Show)

-- Util

putStrLnIO :: MonadIO m => String -> m ()
putStrLnIO = liftIO . putStrLn

printIO :: (Show s, MonadIO m) => s -> m ()
printIO = putStrLnIO . show


profile :: MonadIO m => m b -> m b
profile action = do
  before <- liftIO getCPUTime
  x <- action
  after <- liftIO getCPUTime
  let diff = (fromIntegral (after - before)) / (10^(12::Integer))
  liftIO $ printf "Computation time: %0.3f sec\n" (diff :: Double)
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
  return $ CreateObject objID (Object pose cubeScale)

-- Offset the lights to be on the end of the wands
handLightOffset :: V3 GLfloat
handLightOffset = handOffset * 2


handDimensions :: V3 GLfloat
handDimensions = V3 0.055 0.055 0.5

handleDimensions :: V3 GLfloat
handleDimensions = V3 0.038 0.038 0.1651


-- Offset the hand model outward to feel like wands rather than batons
handOffset :: V3 GLfloat
handOffset = V3 0 0 (((-(handDimensions ^. _z) / 2) - ((handleDimensions ^. _z)/2)))


cubeScale :: GLfloat
cubeScale = 0.4

roomScale :: GLfloat
roomScale = 10

silenceVoice :: MonadIO m => OpenALSource -> m ()
silenceVoice sourceID = 
  alSourcePosition sourceID (V3 0 0 (-10000) :: V3 GLfloat)
