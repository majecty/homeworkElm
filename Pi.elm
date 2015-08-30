module Pi where

import Random
import Signal
import Graphics.Element (Element, empty)
import String
import Window
  -- TODO: modify/add imports as needed

type alias Point = { x:Float, y:Float }

type alias State = ((Int, List Point), (Int, List Point))

initState = ((0,[]), (0,[]))

upstate : Point -> State -> State
upstate pt st =
  -- TODO
  st

view : (Int,Int) -> State -> Element
view (w,h) st =
  -- TODO
  empty

genPoint : Random.Seed -> (Point, Random.Seed)
genPoint s =
  -- TODO
  ({x=0,y=0}, s)

signalPointSeed : Signal (Point, Random.Seed)
signalPointSeed =
  -- TODO
  Signal.constant (genPoint (Random.initialSeed 0))

signalPoint : Signal Point
signalPoint =
  -- TODO
  Signal.constant {x=0,y=0}

main : Signal Element
main =
  Signal.map2 view Window.dimensions
    (Signal.foldp upstate initState signalPoint)
