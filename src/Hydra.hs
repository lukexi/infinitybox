module Hydra where
import System.Hardware.Hydra
import Linear
import Control.Monad.Trans
import Data.Maybe

initHydra :: IO ()
initHydra = do
  _          <- sixenseInit
  _          <- setActiveBase 0
  _          <- autoEnableHemisphereTracking 0 
  return ()

quatFromV4 :: RealFrac a => V4 a -> Quaternion a
quatFromV4 (V4 x y z w) = fmap realToFrac (Quaternion w $ V3 x y z)

getHands :: MonadIO m => m [ControllerData]
getHands = liftIO $ do
  leftHand   <- getNewestData 0
  rightHand  <- getNewestData 1
  return (catMaybes [leftHand, rightHand])

handButtons :: ControllerData -> [Button]
handButtons = activeButtons . buttons