import Linear
import Types

import qualified Data.Map as Map



main = do
    -- print $ Object 0 0 `interpolateObjects` Object 1 1

    let mapA = Map.fromList [(0::Int, Object 0 0)]
    let mapB = Map.fromList [(0::Int, Object 1 1)]
    print $ Map.unionWith interpolateObjects mapA mapB

    print "hi"