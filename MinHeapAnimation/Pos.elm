module Pos where

import Types exposing (..)

makePos : Float -> Float -> Pos
makePos x y = { x = x, y = y }

add : Pos -> Pos -> Pos
add left right = {
    x = left.x + right.x
  , y = left.y + right.y
  }
