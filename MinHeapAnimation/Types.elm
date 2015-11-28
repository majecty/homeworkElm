module Types where

import Graphics.Collage as Collage

type alias Pos = {
    x : Float
  , y : Float
  }

type alias Elem = {
    pos : Pos
  , view : Collage.Form
  }
