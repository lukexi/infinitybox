{-# LANGUAGE RecordWildCards #-}
module Pal.Geometry where 
import Pal.Types
import Pal.ArrayBuffer
import Linear
geometryFromShape Shape{..} = do

  positions   <- bufferData         positionList
  normals     <- bufferData         normalList
  tangents    <- bufferData         tangentList
  uvs         <- bufferData         uvList
  indices     <- bufferElementData  indexList
  
  let vertCount = numVerts

  return Geometry{..}