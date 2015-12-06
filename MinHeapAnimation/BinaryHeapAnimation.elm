module BinaryHeapAnimation where

import Graphics.Collage as Collage
import Types exposing (..)

type Type = Idle
type alias State = {
    type_ : Type
  , deltaTime : DeltaTime
  }

type Animation a = Animation (State -> a)

initState : DeltaTime -> State
initState dt = { type_ = Idle, deltaTime = dt }

empty : a -> Animation a
empty a = Animation <| \_ -> a

run : State -> Animation a -> a
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
