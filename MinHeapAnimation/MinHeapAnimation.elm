module MinHeapAnimation where

import Color
import Graphics.Collage as Collage
import Graphics.Element as Element
import Graphics.Element exposing (Element)

import Box exposing (Box, makeBox)
import Types exposing (..)

main : Element
main =
  let initModel = init
  in
    view initModel

type alias Model = {
    lovelyBox : Box
  , sadBox : Box
  }

init : Model
init =
     {
       lovelyBox = makeBox { x=0, y=0 }
     , sadBox = makeBox { x=50, y=50 }
     }

view : Model -> Element
view {lovelyBox, sadBox} = Collage.collage 500 500 [ lovelyBox.view, sadBox.view ]

