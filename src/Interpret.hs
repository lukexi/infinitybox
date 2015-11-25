{-# LANGUAGE FlexibleContexts #-}
module Interpret where

import Control.Monad.State.Strict

import Control.Lens.Extra
import qualified Data.Map.Strict as Map
import Sound.Pd
import Animation.Pal
import Physics.Bullet
import Graphics.VR.Pal
import Types



-- | Interpret a command into the client's world state.
-- We use a pattern here of Reliable Create/Destroy messages and Unreliable Update
-- messages. We only create and destroy the object when receiving those messages,
-- and only perform updates if the object already exists, since unreliable messages
-- can come in before the object is created and after it is destroyed.

interpret :: (MonadIO m, MonadState World m) => PureData -> VRPal -> Op -> m ()

interpret pd _ (CreateObject objID obj)       = do
  voiceID <- dequeueVoice
  
  wldCubes      . at objID ?= obj
  wldCubeVoices . at objID ?= voiceID

  liftIO $ sendGlobal pd (show voiceID ++ "new-phrase") Bang
  
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


interpret pd _ (DeleteObject objID)           = do
  mVoiceID <- use $ wldCubeVoices . at objID
  forM_ mVoiceID $ \voiceID -> do
    mSourceID <- use $ wldVoiceSources . at voiceID
    forM_ mSourceID silenceVoice
  wldCubes      . at objID .= Nothing
  wldCubeVoices . at objID .= Nothing

interpret _ _ (UpdateObject objID obj)       = 
  wldCubes   . at objID    . traverse .= obj

interpret _ _ (UpdatePlayer playerID player) = 
  wldPlayers . at playerID . traverse .= player

interpret _ _ (Connect playerID player)      = do
  wldPlayers . at playerID ?= player
  putStrLnIO (playerID ++ " connected")
  
interpret _ _ (Disconnect playerID)          = do
  wldPlayers . at playerID .= Nothing
  putStrLnIO (playerID ++ " disconnected")

interpret _ vrPal (ObjectCollision collision) = do
  let bodyIDs  = fromIntegral <$> sequence [cbBodyAID, cbBodyBID] collision
      impulse  = cbAppliedImpulse collision
      axis     = 0
      duration = floor (10000 * realToFrac impulse)
  when (leftHandRigidBodyID `elem` bodyIDs) $ do
    let controllerNumber = 0
    triggerHandHapticPulse vrPal controllerNumber axis duration
  when (rightHandRigidBodyID `elem` bodyIDs) $ do
    let controllerNumber = 1
    triggerHandHapticPulse vrPal controllerNumber axis duration

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
interpret _ _ Restart = return ()