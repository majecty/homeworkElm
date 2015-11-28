module MinHeapAnimation where

import Color
import Graphics.Collage as Collage
import Graphics.Element as Element
import Graphics.Element exposing (Element)

main' : Element
main' = Element.show 3

main : Element
main = Collage.collage 500 500 [ myRect ]

type alias Elem = {
    x : Float
  , y : Float
  , view : Collage.Form
  }

type alias Model = {
    lovelyBox : Elem
  }

view : Model -> Element
view {lovelyBox} = Collage.collage 500 500 [ lovelyBox.view ]
 
myRect : Collage.Form
myRect = Collage.filled Color.black <| Collage.rect 100 100
