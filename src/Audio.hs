{-# LANGUAGE FlexibleContexts #-}
module Audio where
import Sound.Pd1
import Game.Pal
import qualified Data.Map.Strict as Map
import Linear
import Graphics.GL
import Types
import Network.UDP.Pal
import Control.Lens
import Control.Monad.State
import Control.Concurrent.STM

exhaustChanIO :: MonadIO m => TChan a -> m [a]
exhaustChanIO = liftIO . atomically . exhaustChan

initAudio :: IO [(OpenALSource, Int, TChan Message)]
initAudio = do
  -- Set up sound
  addToLibPdSearchPath "audio-prototypes/infinity3"
  _main <- makePatch "audio-prototypes/infinity3/main"
  openALSources <- getPdSources
  patches <- foldM (\accum sourceID -> do
    -- Set each voice to a unique channel from 1-16
    let voiceID = length accum + 1

    alSourcePosition sourceID (V3 0 0 (-10000) :: V3 GLfloat)

    output <- makeReceiveChan (show voiceID ++ "tick")
    return (accum ++ [(sourceID, voiceID, output)])
    ) [] openALSources
  return patches

updateAudio :: (MonadIO m, MonadState World m) => [(OpenALSource, t, TChan Message)] -> m ()
updateAudio patches = do

  Pose totalHeadPosit totalHeadOrient <- totalHeadPose
  alListenerPosition totalHeadPosit
  alListenerOrientation totalHeadOrient

  cubes <- use wldCubes
  forM_ (zip (Map.toList cubes) patches) $ \((cubeID, cubeObj), (sourceID, _patch, output)) -> do
    alSourcePosition sourceID (cubeObj ^. objPose . posPosition)
    exhaustChanIO output >>= mapM_ (\val -> 
      case val of
        Bang -> wldPatchOutput . at cubeID ?== 1
        _ -> return ()
      )
    wldPatchOutput . at cubeID . traverse -= 0.01
  -- Pass hand positions to OpenAL sources
  --handWorldPoses <- use (wldPlayer . plrHandPoses)
  --forM_ (zip openALSources handWorldPoses) $ \(sourceID, Pose posit _orient) -> do
  --  alSourcePosition sourceID posit

