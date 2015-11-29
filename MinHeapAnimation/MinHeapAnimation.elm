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

import Box exposing (Box, makeBox)
import Circle exposing (Circle, makeCircle)

main : Signal Element
main = Signal.map view modelAtFrame

--main : Element
--main = Element.show <| Box.move {x=1, y=0} <| makeBox {x=1, y=1}

type alias Model = {
    circle : Circle
  }

initModel : Model
initModel =
     {
       circle = makeCircle { x = 0, y = 10 }
     }

fps : Signal Time
fps = Time.fps 30

modelAtFrame : Signal Model
modelAtFrame = Signal.foldp updateModel initModel fps

type alias DeltaTime = Time
updateModel : DeltaTime -> Model -> Model
updateModel dt prevModel = prevModel

view : Model -> Element
view {circle} = Collage.collage 500 500 [ circle.view ]

