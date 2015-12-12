module Types where

import Time exposing (Time)
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

type alias DeltaTime = Time

type alias Frame = {
    dt : DeltaTime
  , insert : Bool
  }
