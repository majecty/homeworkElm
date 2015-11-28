module Box(Box, makeBox, move) where

import Color exposing (Color)
import Graphics.Collage as Collage

import Pos as Pos
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

move : Vector -> Box -> Box
move movement prevBox =
  let prevPos = prevBox.pos
      newPos = Pos.add prevPos movement
  in
     updateView <| { prevBox | pos = newPos }

makeView : Pos -> Collage.Form
makeView {x, y} = Collage.move (x, y) <| Collage.filled Color.black <| Collage.rect 100 100

updateView : Box -> Box
updateView box =
  let view = makeView box.pos
  in
     { box | view = view }
