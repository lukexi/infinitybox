{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module CompressedQuaternion where
import Data.Binary
import GHC.Generics
import Linear
import GHC.Int

-- | Squeeze a Quaternion into 6 bytes by dropping w 
-- (which can be recalculated)
-- and using 16bits for each value

data CompressedQuaternion = CompressedQuaternion !Int16 !Int16 !Int16
    deriving (Generic, Binary, Show)

compressQuaternion :: (RealFrac a) => Quaternion a -> CompressedQuaternion
compressQuaternion (Quaternion _ (V3 x y z)) = 
    CompressedQuaternion 
        (compress x) 
        (compress y) 
        (compress z)
    where maxInt16 = realToFrac (maxBound :: Int16)
          compress = floor . (* maxInt16)

decompressQuaternion (CompressedQuaternion cx cy cz) =
    Quaternion w 
        (V3 x y z)
    where maxInt16 = realToFrac (maxBound :: Int16)
          w = sqrt (1 - x^2 - y^2 - z^2)
          x = realToFrac cx / maxInt16 
          y = realToFrac cy / maxInt16 
          z = realToFrac cz / maxInt16

