module BinaryHeapAnimation(initState, empty, run, animateElement) where

import Graphics.Collage as Collage
import Types exposing (..)

import BinaryHeapTypes exposing(..)

initState : DeltaTime -> AnimationState
initState dt = { type_ = Idle, deltaTime = dt }

empty : a -> Animation a
empty a = Animation <| \_ -> a

run : AnimationState -> Animation a -> a
run state (Animation job) = job state

animateElement : Collage.Form -> Animation Collage.Form
animateElement form = Animation <| \state -> form

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
