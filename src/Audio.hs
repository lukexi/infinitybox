{-# LANGUAGE FlexibleContexts #-}
module Audio where
import Sound.Pd1
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Linear.Extra
-- import Graphics.GL
import Types
import Network.UDP.Pal
import Control.Lens.Extra
import Control.Monad.State
import Control.Concurrent.STM


exhaustChanIO :: MonadIO m => TChan a -> m [a]
exhaustChanIO = liftIO . atomically . exhaustChan

initAudio :: IO (TChan Message, TChan Message, Map VoiceID OpenALSource)
initAudio = do

  -- OpenAL-soft annoyingly needs its hrtf files in a certain APPDATA subfolder,
  -- so copy them here
  _ <- copyOpenALHRTFs

  -- Set up sound
  addToLibPdSearchPath "patches/kit"
  addToLibPdSearchPath "patches/kit/list-abs"
  addToLibPdSearchPath "patches"
  addToLibPdSearchPath "audio-prototypes/infinity6"
  _main <- makePatch   "audio-prototypes/infinity6/percy"
  
  -- Associate each voice number with an OpenAL source
  openALSources <- getPdSources

  let voiceSources = init openALSources -- reserve last source for the logo sound
      sourcesByVoice = Map.fromList (zip [1..] voiceSources)

  alSourcePosition (last openALSources) (logoObject ^. objPose . posPosition)

  -- Initially send voices to very far away to silence them
  forM_ voiceSources silenceVoice

  alListenerGain (3::Double)

  pitchesByVoice    <- makeReceiveChan "pitchesByVoice"
  amplitudesByVoice <- makeReceiveChan "amplitudesByVoice"
  
  return (pitchesByVoice, amplitudesByVoice, sourcesByVoice)

updateAudio :: (MonadIO m, MonadState World m) => TChan Message -> TChan Message -> m ()
updateAudio pitchesByVoice amplitudesByVoice = do

  sendGlobal "dayNight" =<< Atom . Float . dayNightCycleAt <$> use wldTime

  -- Update OpenAL Listener from player's total head pose
  alListenerPose =<< totalHeadPose <$> use wldPlayer

  -- Set voice levels to 1 when they tick
  exhaustChanIO pitchesByVoice >>= mapM_ (\val -> 
    case val of
      List [Float voiceID, Float pitch] -> wldVoicePitch . at (floor voiceID) ?= pitch
      _ -> return ()
    )

  exhaustChanIO amplitudesByVoice >>= mapM_ (\val -> 
    case val of
      List [Float voiceID, Float amp]  -> wldVoiceAmplitude . at (floor voiceID) ?= amp
      _ -> return ()
    )

  -- -- Kick is always centered in the floor
  -- kickVoiceID <- use wldKickVoiceID
  -- mKickSourceID <- use $ wldVoiceSources . at kickVoiceID
  -- forM_ mKickSourceID $ \sourceID -> 
  --   alSourcePosition sourceID (V3 0 (-1) 0 :: V3 GLfloat)

  -- Update voices with cube positions
  cubes <- use wldCubes
  forM_ (Map.toList cubes) $ \(cubeID, cubeObj) -> do
    mVoiceID  <- use $ wldCubeVoices . at cubeID
    forM_ mVoiceID $ \voiceID -> do
      mSourceID <- use $ wldVoiceSources . at voiceID
      forM_ mSourceID $ \sourceID -> do
    
        alSourcePosition sourceID (cubeObj ^. objPose . posPosition)

        -- This is a little expensive, as it's getting the pd ref and sending a list per cube.
        -- sendGlobal (show voiceID ++ "xyz") $ 
        --   List (map realToFrac (toList (cubeObj ^. objPose . posPosition)))


