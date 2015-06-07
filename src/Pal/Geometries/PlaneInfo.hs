{-# LANGUAGE RecordWildCards #-}

module Pal.Geometries.PlaneInfo where

import Graphics.GL
import Graphics.GL.Pal

import Pal.WithActions
import Pal.ArrayBuffer
import Pal.Types


--initPlaneGeometry :: GLProgram -> IO Geometry
initPlaneGeometry scale = do

  -----------------
  -- Plane Positions
  -----------------
  
  -- Buffer the plane vertices
  positions <- bufferData $ map ( * scale )


      -- front
      [ -1.0 ,  1.0 ,  0.0
      , -1.0 , -1.0 ,  0.0 
      ,  1.0 ,  1.0 ,  0.0

      ,  1.0 , -1.0 ,  0.0
      ,  1.0 ,  1.0 ,  0.0
      , -1.0 , -1.0 ,  0.0

      ]




  -----------------
  -- Plane Normals
  -----------------

  -- Buffer the plane vertices
  normals <- bufferData $ take 18 $ cycle [0,0,1]


  -----------------
  -- Plane Tangents
  -----------------

  -- Buffer the plane vertices
  tangents <- bufferData $ take 18 $ cycle [1,0,0]



  -----------------
  -- Plane UVs
  -----------------

  -- Buffer the plane vertices
  uvs <- bufferData 

      -- front
      [  0.0 ,  1.0
      ,  0.0 ,  0.0
      ,  1.0 ,  1.0

      ,  1.0 ,  0.0 
      ,  1.0 ,  1.0 
      ,  0.0 ,  0.0 

      ]


  let vertCount = 3 * 2

  return Geometry{..} 

