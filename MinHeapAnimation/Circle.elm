module Circle(Circle, makeCircle, move) where

import Color exposing (Color)
import Graphics.Collage as Collage

import Pos as Pos
import Pos exposing (makePos)
import Types exposing (..)

type alias Circle = {
    pos : Pos
  , view : Collage.Form

  , color : Color
  }

makeCircle : Pos -> Circle
makeCircle {x, y} =
  let pos = makePos x y 
      color = Color.black
      view = makeView pos
  in
     { pos = pos, view = view, color = color }

move : Vector -> Circle -> Circle
move movement prevCircle =
  let prevPos = prevCircle.pos
      newPos = Pos.add prevPos movement
  in
     updateView <| { prevCircle | pos = newPos }

makeView : Pos -> Collage.Form
makeView {x, y} = Collage.move (x, y) <| Collage.filled Color.orange <| Collage.circle 30

updateView : Circle -> Circle
updateView box =
  let view = makeView box.pos
  in
     { box | view = view }
