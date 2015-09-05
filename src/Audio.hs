{-# LANGUAGE FlexibleContexts #-}
module Audio where
import Sound.Pd1
import Game.Pal
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Linear
import Graphics.GL
import Types
import Network.UDP.Pal
import Control.Lens
import Control.Monad.State
import Control.Concurrent.STM
import Data.Foldable
import Data.Traversable

exhaustChanIO :: MonadIO m => TChan a -> m [a]
exhaustChanIO = liftIO . atomically . exhaustChan



initAudio :: IO (TChan Message, Map VoiceID OpenALSource)
initAudio = do
  -- Set up sound
  addToLibPdSearchPath "audio-prototypes/infinity5"
  _main <- makePatch "audio-prototypes/infinity5/phasy2"
  
  -- Associate each voice number with an OpenAL source
  openALSources <- getPdSources

  let sourcesByVoice = Map.fromList (zip [0..] openALSources)

  forM_  openALSources $ \sourceID -> 
    
    -- Initially send voices to very far away to silence them
    alSourcePosition sourceID (V3 0 0 (-10000) :: V3 GLfloat)

  ticks <- makeReceiveChan "voiceTicks"
  
  return (ticks, sourcesByVoice)

updateAudio :: (MonadIO m, MonadState World m) => TChan Message -> m ()
updateAudio ticks = do

  -- Update OpenAL Listener from player's total head pose
  player <- use wldPlayer
  let Pose totalHeadPosit totalHeadOrient = totalHeadPose player
  alListenerPosition totalHeadPosit
  alListenerOrientation totalHeadOrient


  exhaustChanIO ticks >>= mapM_ (\val -> 
    case val of
      Atom (Float voiceID) -> wldVoiceOutput . at (floor voiceID) ?== 1          
      _ -> return ()
    )

  cubes <- use wldCubes
  forM_ (Map.toList cubes) $ \(cubeID, cubeObj) -> do
    mVoiceID  <- use $ wldCubeVoices . at cubeID
    forM_ mVoiceID $ \voiceID -> do
      mSourceID <- use $ wldVoiceSources . at voiceID
      forM_ mSourceID $ \sourceID -> do
    
        alSourcePosition sourceID (cubeObj ^. objPose . posPosition)

        liftIO $ sendGlobal (show voiceID ++ "xyz") $ List (map realToFrac (toList (cubeObj ^. objPose . posPosition)))
        
        wldVoiceOutput . at voiceID . traverse -= 0.01
        wldVoiceOutput . at voiceID . traverse %= max 0


