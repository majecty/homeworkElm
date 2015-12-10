module BinaryHeap.Types where

import BinaryHeap.BinaryHeap as BinaryHeap
import Types exposing (..)

type alias InternalHeap = BinaryHeap.Heap

type Model =
    Stable {
      heap : InternalHeap
    , animationState : AnimationState
    }

  | Transition {
      beforeHeap : InternalHeap
    , afterHeap : InternalHeap
    , animationState : AnimationState
    }

type AnimationType = Idle
type alias AnimationState = {
    type_ : AnimationType
  , deltaTime : DeltaTime
  }

type Animation a = Animation (AnimationState -> a)
