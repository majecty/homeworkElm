module Boundary(getBoundary, middlePos) where

import Types exposing (..)

floatMax : Float
floatMax = 3.402823e+38
floatMin : Float
floatMin = -3.402823e+38

initialBoundary : Boundary
initialBoundary = {
    minX = floatMax
  , maxX = floatMin
  , minY = floatMax
  , maxY = floatMin
  }

updateBoundary : Pos -> Boundary -> Boundary
updateBoundary {x, y} {minX, maxX, minY, maxY} = {
    minX = min minX x
  , maxX = max maxX x
  , minY = min minY y
  , maxY = max maxY y
  }

getBoundary : List Pos -> Boundary
getBoundary poses =
     List.foldl updateBoundary initialBoundary poses

middlePos : Boundary -> Pos
middlePos {minX, maxX, minY, maxY} = {
    x = (minX + maxX) / 2
  , y = (minY + maxY) / 2
  }
