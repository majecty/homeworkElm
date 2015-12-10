module BinaryHeap.Animation(initState, empty, run, animateElement, merge, map, update) where

import Debug
import Graphics.Collage as Collage
import Time

import BinaryHeap.Types exposing(..)
import Types exposing (..)

initState : DeltaTime -> AnimationState
initState dt = { type_ = Idle, deltaTime = dt }

update : DeltaTime -> AnimationState -> AnimationState
update dt prevModel = { prevModel | deltaTime = prevModel.deltaTime + dt }

empty : a -> Animation a
empty a = Animation <| \_ -> a

run : AnimationState -> Animation a -> a
run state (Animation job) = job state

animateElement : Collage.Form -> Animation Collage.Form
animateElement form = Animation <| (\{deltaTime} ->
  Collage.scale (dtToScale deltaTime) form)

modular : Float -> Float -> Float
modular a b =
  let divResult = toFloat <| floor (a / b)
  in
    a - (divResult * b)

dtToScale : DeltaTime -> Float
dtToScale dt =
  let seconds = dt / Time.second
      fraction = (seconds `modular` 1)
  in
    0.9 + fraction * 0.1

merge : List (Animation a) -> Animation (List a)
merge animations = Animation <| \state ->
  List.map (run state) animations

map : (a -> b) -> Animation a -> Animation b
map f animationA = Animation <| \state ->
  run state animationA |> f

{-|
  animation
  1. tree animation.
  2. ui animation.
    let's do it later.
-}

{-|
  idle animation
  blink every seconds.
-}

{-|
  bubbleDown
    1. show three node blining
-}
