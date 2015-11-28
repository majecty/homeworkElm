module MinHeapAnimation where

import Color
import Graphics.Collage as Collage
import Graphics.Element as Element
import Graphics.Element exposing (Element)
import Signal as Signal
import Signal exposing (Signal)
import Time as Time
import Time exposing (Time)

import Box exposing (Box, makeBox)
import Types exposing (..)

main : Signal Element
main = Signal.map view modelAtFrame

--main : Element
--main = Element.show <| Box.move {x=1, y=0} <| makeBox {x=1, y=1}

type alias Model = {
    lovelyBox : Box
  , sadBox : Box
  }

initModel : Model
initModel =
     {
       lovelyBox = makeBox { x=0, y=0 }
     , sadBox = makeBox { x=50, y=50 }
     }

fps : Signal Time
fps = Time.fps 30

modelAtFrame : Signal Model
modelAtFrame = Signal.foldp updateModel initModel fps

type alias DeltaTime = Time
updateModel : DeltaTime -> Model -> Model
updateModel dt prevModel =
  let {lovelyBox} = prevModel
      newLovelyBox = Box.move { x = 1, y=0 } lovelyBox
  in
     { prevModel | lovelyBox = newLovelyBox }

view : Model -> Element
view {lovelyBox, sadBox} = Collage.collage 500 500 [ lovelyBox.view, sadBox.view ]

