{-# LANGUAGE RecordWildCards #-}

module Pal.Geometries.Plane where

import Graphics.GL

import Pal.Types
import Pal.Geometry

import Linear       hiding ( trace   )
import Control.Lens hiding ( indices )
import Data.Foldable
import Debug.Trace

fI :: ( Integral a , Num b ) => a -> b
fI = fromIntegral

planeShape :: V2 GLfloat -> V3 GLfloat -> V3 GLfloat -> V2 Int -> Shape
planeShape size normal up subdivisions = Shape{..}

  where

    numVerts      = traceShowId $ 3 * 2 * ( fI subdivisionsX  ) * ( fI subdivisionsY )
    numPoints     = traceShowId $ ( fI subdivisionsX + 1 ) * ( fI subdivisionsY + 1 )

    subdivisionsX = subdivisions ^. _x 
    subdivisionsY = subdivisions ^. _y 

    sizeX         = size ^. _x 
    sizeY         = size ^. _y 

    tangent       = normalize $ cross normal up 
    binormal      = normalize $ cross tangent normal
    
    norArray      = take ( fI numPoints * 3 ) $ cycle [ normal  ^. _x , normal  ^. _y , normal  ^. _z ]
    tanArray      = take ( fI numPoints * 3 ) $ cycle [ tangent ^. _x , tangent ^. _y , tangent ^. _z ]

    posArray      = makePlanePositions sizeX sizeY tangent binormal subdivisionsX subdivisionsY
    uvArray       = makePlaneUVs                                    subdivisionsX subdivisionsY
    indexArray    = makePlaneIndices                                subdivisionsX subdivisionsY


    positionList  = posArray
    normalList    = norArray
    tangentList   = tanArray
    uvList        = uvArray

    indexList     = indexArray   

planeGeometry size normal up subdivisions = geometryFromShape $ planeShape size normal up subdivisions 


makePlanePositions :: GLfloat -> GLfloat -> V3 GLfloat -> V3 GLfloat -> Int -> Int -> [ GLfloat ]
makePlanePositions xSize ySize xVec yVec subdivisionsX subdivisionsY = positions
  where

    positions = concat [ getPoint x  y | x <- [ 0 .. subdivisionsX ] , y <- [ 0 .. subdivisionsY ] ]
    getPoint x y = toList p
      where 
        p = xVec * ( fI x / fI subdivisionsX ) * (realToFrac xSize)
          + yVec * ( fI y / fI subdivisionsY ) * (realToFrac ySize)
          - 0.5 * xVec * (realToFrac xSize)
          - 0.5 * yVec * (realToFrac ySize)


makePlaneUVs :: Int -> Int -> [ GLfloat ]
makePlaneUVs subdivisionsX subdivisionsY = uvs
  where

    uvs = concat [ getPoint x  y | x <- [ 0 .. subdivisionsX ] , y <- [ 0 .. subdivisionsY ] ]
    getPoint x y = toList p
      where 
        p = V2 ( fI x  / fI subdivisionsX )  ( fI y / fI subdivisionsY )


makePlaneIndices :: Int -> Int -> [ GLuint ]
makePlaneIndices subdivisionsX subdivisionsY = map fI indices
  where
    indices = concat [ getIndices x  y | x <- [ 0 .. subdivisionsX - 1 ] , y <- [ 0 .. subdivisionsY - 1 ] ]
    getIndices x y = [ p1 , p3 , p2 , p2 , p3 , p4]
      where 

        p1 = ( x + 0 ) * ( subdivisionsX + 1 ) + ( y + 0 )   
        p3 = ( x + 0 ) * ( subdivisionsX + 1 ) + ( y + 1 )  
        p2 = ( x + 1 ) * ( subdivisionsX + 1 ) + ( y + 0 ) 
        p4 = ( x + 1 ) * ( subdivisionsX + 1 ) + ( y + 1 ) 
