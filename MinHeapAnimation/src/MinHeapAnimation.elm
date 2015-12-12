module MinHeapAnimation where

{-|
  Show min heap animations.
-}
import Graphics.Collage as Collage
import Graphics.Element as Element
import Graphics.Element exposing (Element)
import Graphics.Input as Input
import Signal as Signal
import Signal exposing (Signal)

import BinaryHeap.Model as BinaryHeapModel
import BinaryHeap.Types as BinaryHeapTypes
import BinaryHeap.Viewer as BinaryHeapViewer
import Circle exposing (Circle, makeCircle)
import Global
import Input
import Types exposing (..)
import UI.Button as Button

main : Signal Element
main = Signal.map view modelAtFrame

--main : Element
--main = Element.show <| Box.move {x=1, y=0} <| makeBox {x=1, y=1}

type alias Model = {
    circle : Circle
  , heap : BinaryHeapTypes.Model
  }

initModel : Model
initModel =
     {
       circle = makeCircle { x = 0, y = 10 }
     , heap = BinaryHeapModel.makeHeap [1..50]
     }

modelAtFrame : Signal Model
modelAtFrame = Signal.foldp (\event model -> updateModel event model) initModel Input.inputEvent

updateModel : Frame -> Model -> Model
updateModel {dt, insert} prevModel = case insert of
  False -> {
      circle = prevModel.circle
    , heap = BinaryHeapModel.update dt prevModel.heap
    }
  True ->
    let insertedHeap = BinaryHeapModel.insert 0 prevModel.heap
    in {
        circle = prevModel.circle
      , heap = BinaryHeapModel.update dt insertedHeap
      }

view : Model -> Element
view {circle, heap} =
  let tree =
        Collage.collage Global.width Global.height [
          circle.view
        , BinaryHeapViewer.view heap
        ]
      button =
        Input.button
          (Signal.message
            (.address Button.message)
            Button.Nothing)
          "insert"
    in
      Element.beside tree button

{-|
 Binary heap animation.
 bubbleDown
 bubbleUp

 deleteMin :
 1: take last element as x
 2: set first element to x
 3: bubble down from first element

 bubbleDown :
 1: compar i and two children
 2: find minimum of i and two children
 3: if i is minimum then exit
    else swap minimum and i
-}
