module MinHeapAnimation where

{-|
  Show min heap animations.
-}
import Graphics.Collage as Collage
import Graphics.Element as Element
import Graphics.Element exposing (Element)
import Signal as Signal
import Signal exposing (Signal)
import Time as Time
import Time exposing (Time)

import Circle exposing (Circle, makeCircle)
import Global
import Types exposing (..)

-- copied from homework
import BinaryHeap
import BinaryHeapAnimation
import BinaryHeapViewer

main : Signal Element
main = Signal.map view modelAtFrame

--main : Element
--main = Element.show <| Box.move {x=1, y=0} <| makeBox {x=1, y=1}

type alias Model = {
    circle : Circle
  , heap : BinaryHeap.Heap
  }

makeHeap : List Int -> BinaryHeap.Heap
makeHeap xs = case xs of
  [] -> BinaryHeap.empty
  x::xs' -> BinaryHeap.insert x <| makeHeap xs'

initModel : Model
initModel =
     {
       circle = makeCircle { x = 0, y = 10 }
     , heap = makeHeap [1..50]
     }

fps : Signal Time
fps = Time.fps 30

modelAtFrame : Signal (DeltaTime, Model)
modelAtFrame = Signal.foldp (\t (_, model) -> (t, updateModel t model)) (0, initModel) fps

updateModel : DeltaTime -> Model -> Model
updateModel dt prevModel = prevModel

view : (DeltaTime, Model) -> Element
view (dt, {circle, heap}) =
  Collage.collage Global.width Global.height [
    circle.view
  , BinaryHeapAnimation.run (BinaryHeapAnimation.initState dt) <| BinaryHeapViewer.view heap
  ]

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
