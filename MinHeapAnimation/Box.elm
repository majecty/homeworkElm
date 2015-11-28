module Box(Box, makeBox) where

import Color exposing (Color)
import Graphics.Collage as Collage

import Pos exposing (makePos)
import Types exposing (..)

type alias Box = {
    pos : Pos
  , view : Collage.Form

  , color : Color
  }

makeBox : Pos -> Box
makeBox {x, y} =
  let pos = makePos x y 
      color = Color.black
      view = makeView pos
  in
     { pos = pos, view = view, color = color }

makeView : Pos -> Collage.Form
makeView {x, y} = Collage.move (x, y) <| Collage.filled Color.black <| Collage.rect 100 100
