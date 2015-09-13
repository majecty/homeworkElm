module Pi where

import Random
import Signal
import Graphics.Element exposing (Element, empty, show)
import String
import Window
import Text
import Time

type alias Point = { x:Float, y:Float }

type alias State = ((Int, List Point), (Int, List Point))

radius : number 
radius = 3

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

missCount : State -> Int
missCount ((_, _), (count, _)) = count

view : (Int,Int) -> State -> Element
view (w,h) state =
  let total = (toFloat <| missCount <| state) + (toFloat <| hitCount <| state) in
  let ratio = (toFloat <| hitCount <| state) / total in
  show ("pi is " ++ (toString (ratio * 4)))

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
