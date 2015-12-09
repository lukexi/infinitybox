{-# LANGUAGE FlexibleContexts #-}
module Audio where
import Sound.Pd
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Linear.Extra
-- import Graphics.GL
import Types
import Network.UDP.Pal
import Control.Lens.Extra hiding (List)
import Control.Monad.State
import Control.Concurrent.STM

exhaustChanIO :: MonadIO m => TChan a -> m [a]
exhaustChanIO = liftIO . atomically . exhaustChan

initAudio :: PureData -> IO (Map VoiceID OpenALSource)
initAudio pd = do

  -- OpenAL-soft annoyingly needs its hrtf files in a certain APPDATA subfolder,
  -- so copy them here
  _ <- copyOpenALHRTFs

  -- Set up sound
  forM_ ["patches", "patches/kit", "patches/kit/list-abs"] 
    (addToLibPdSearchPath pd)
  _main <- makePatch pd "patches/percy"

  -- Use an exponential falloff rate to magnify proximity effects
  setOpenALDistanceModelExponent
  
  -- Associate each voice number with an OpenAL source
  let openALSources  = pdSources pd
      voiceSources   = init openALSources -- reserve last source for the logo sound
      sourcesByVoice = Map.fromList (zip [1..] voiceSources)

  alSourcePosition (last openALSources) (logoObject ^. objPose . posPosition)

  -- Initially send voices to very far away to silence them
  forM_ voiceSources silenceVoice

  alListenerGain (3::Double)
  
  return sourcesByVoice

updateAudio :: (MonadIO m, MonadState World m) => PureData -> m ()
updateAudio pd = do

  sendGlobal pd "dayNight" =<< Atom . Float . dayNightCycleAt <$> use wldTime

  -- Update OpenAL Listener from player's total head pose
  alListenerPose =<< totalHeadPose <$> use wldPlayer

  mPitches    <- liftIO $ readArray pd "pitches"    (0::Int) 15
  mAmplitudes <- liftIO $ readArray pd "amplitudes" (0::Int) 15

  forM_ mPitches $ \pitches -> 
    forM_ mAmplitudes $ \amplitudes -> 
      forM_ (zip3 [1..] pitches amplitudes) $ \(voiceID, pitch, amp) -> do
        wldVoicePitch     . at voiceID ?= pitch
        wldVoiceAmplitude . at voiceID ?= amp
      

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


