module Pi where

import Random
import Signal
import Graphics.Element exposing (Element, empty, show)
import Graphics.Collage as Collage
import Graphics.Collage exposing (Form)
import String
import Window
import Text
import Time
import Color
import Color exposing (Color)
import List exposing (map)

type alias Point = { x:Float, y:Float }

type alias Size = Float

type alias State = ((Int, List Point), (Int, List Point))

radius : number 
radius = 1

initState = ((0,[]), (0,[]))

upstate : Point -> State -> State
upstate pt ((hitCount, hitList), (missCount, missList)) =
  if | isInCircle pt -> ((hitCount + 1, pt::hitList), (missCount, missList))
     | otherwise -> ((hitCount, hitList), (missCount + 1, pt::missList))

isInCircle : Point -> Bool
isInCircle pt =
  let distanceSquareFromZero = pt.x * pt.x + pt.y * pt.y in
  distanceSquareFromZero < radius * radius

hitCount : State -> Int
hitCount ((count, _), (_, _)) = count

hitPoints : State -> List Point
hitPoints ((_, points), _) = points

missCount : State -> Int
missCount ((_, _), (count, _)) = count

missPoints : State -> List Point
missPoints (_, (_, points)) = points

makeCircleForms : Size -> Color -> List Point -> List Form
makeCircleForms size color points = map (circleToForm size color) points

circleToForm : Size -> Color -> Point -> Form
circleToForm size color point = Collage.circle 3 |> Collage.filled color |> Collage.move (point.x * size, point.y * size)

view : (Int,Int) -> State -> Element
view (w,h) state =
  let total = (toFloat <| missCount <| state) + (toFloat <| hitCount <| state) in
  let ratio = (toFloat <| hitCount <| state) / total in
  let floatW = toFloat w in
  let floatH = toFloat h in
  let minOfWOrH = toFloat <| min w h in
  let size = minOfWOrH / 2 in

  let outerRect = Collage.filled Color.lightOrange (Collage.rect size size) in
  let debugForm = Collage.move (0, size / 2 + 10) <| Collage.toForm <| show ("pi is " ++ (toString (ratio * 4))) in
  let hitCircleForms = makeCircleForms (size / 2) Color.lightPurple <| hitPoints <| state in
  let missCircleForms = makeCircleForms (size / 2) Color.red <| missPoints <| state in

  Collage.collage w h (outerRect::debugForm::(List.append hitCircleForms missCircleForms))

genPoint : Random.Seed -> (Point, Random.Seed)
genPoint s =
  let generator = Random.float -radius radius in
  let (x, s2) = Random.generate generator s in
  let (y, s3) = Random.generate generator s2 in
  ({x=x, y=y}, s3)

signalPointSeed : Signal (Point, Random.Seed)
signalPointSeed =
  let initial = genPoint (Random.initialSeed 0) in
  Signal.foldp (\_ (_, seed) -> genPoint seed) initial (Time.fps 30)

signalPoint : Signal Point
signalPoint =
  Signal.map (\(p, _) -> p) signalPointSeed

main : Signal Element
main =
  Signal.map2 view Window.dimensions
    (Signal.foldp upstate initState signalPoint)
