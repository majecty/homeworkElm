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

-- copied from homework
import BinaryHeap
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

modelAtFrame : Signal Model
modelAtFrame = Signal.foldp updateModel initModel fps

type alias DeltaTime = Time
updateModel : DeltaTime -> Model -> Model
updateModel dt prevModel = prevModel

view : Model -> Element
view {circle, heap} = Collage.collage Global.width Global.height [
    circle.view
  , BinaryHeapViewer.view heap
  ]

