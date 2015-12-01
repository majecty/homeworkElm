module Types where

import Graphics.Collage as Collage

type alias Pos = {
    x : Float
  , y : Float
  }

type alias Vector = Pos

type alias Elem = {
    pos : Pos
  , view : Collage.Form
  }

type alias Boundary = {
    minX : Float
  , maxX : Float
  , minY : Float
  , maxY : Float
  }
