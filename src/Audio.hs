{-# LANGUAGE FlexibleContexts #-}
module Audio where
import Sound.Pd1
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Linear.Extra
import Graphics.GL
import Types
import Network.UDP.Pal
import Control.Lens.Extra
import Control.Monad.State
import Control.Concurrent.STM
import Data.Foldable

exhaustChanIO :: MonadIO m => TChan a -> m [a]
exhaustChanIO = liftIO . atomically . exhaustChan



initAudio :: IO (TChan Message, Map VoiceID OpenALSource)
initAudio = do
  -- Set up sound
  addToLibPdSearchPath "patches/kit"
  addToLibPdSearchPath "patches/kit/list-abs"
  addToLibPdSearchPath "patches"
  addToLibPdSearchPath "audio-prototypes/infinity6"
  _main <- makePatch   "audio-prototypes/infinity6/percy"
  
  -- Associate each voice number with an OpenAL source
  openALSources <- getPdSources

  let sourcesByVoice = Map.fromList (zip [1..] openALSources)

  forM_  openALSources $ \sourceID -> 
    
    -- Initially send voices to very far away to silence them
    alSourcePosition sourceID (V3 0 0 (-10000) :: V3 GLfloat)

  pitchesByVoice <- makeReceiveChan "pitchesByVoice"
  
  return (pitchesByVoice, sourcesByVoice)

updateAudio :: (MonadIO m, MonadState World m) => TChan Message -> m ()
updateAudio pitchesByVoice = do

  -- Update OpenAL Listener from player's total head pose
  alListenerPose =<< totalHeadPose <$> use wldPlayer

  -- Set voice levels to 1 when they tick
  exhaustChanIO pitchesByVoice >>= mapM_ (\val -> 
    case val of
      List [Float voiceID, Float pitch] -> wldVoiceOutput . at (floor voiceID) ?= pitch
      _ -> return ()
    )

  -- -- Decrement each voice by a tiny bit each frame
  -- voiceIDs <- Map.keys <$> use wldVoiceSources
  -- forM_ voiceIDs $ \voiceID -> do
  --   wldVoiceOutput . at voiceID . traverse -= 0.01
  --   wldVoiceOutput . at voiceID . traverse %= max 0

  -- Kick is always centered in the floor
  kickVoiceID <- use wldKickVoiceID
  mKickSourceID <- use $ wldVoiceSources . at kickVoiceID
  forM_ mKickSourceID $ \sourceID -> 
    alSourcePosition sourceID (V3 0 (-1) 0 :: V3 GLfloat)

  -- Update voices with cube positions
  cubes <- use wldCubes
  forM_ (Map.toList cubes) $ \(cubeID, cubeObj) -> do
    mVoiceID  <- use $ wldCubeVoices . at cubeID
    forM_ mVoiceID $ \voiceID -> do
      mSourceID <- use $ wldVoiceSources . at voiceID
      forM_ mSourceID $ \sourceID -> do
    
        alSourcePosition sourceID (cubeObj ^. objPose . posPosition)

        liftIO $ sendGlobal (show voiceID ++ "xyz") $ 
          List (map realToFrac (toList (cubeObj ^. objPose . posPosition)))


