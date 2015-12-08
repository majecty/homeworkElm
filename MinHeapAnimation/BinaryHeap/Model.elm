module BinaryHeap.Model where

import BinaryHeap.BinaryHeap as BinaryHeap
import BinaryHeap.Types exposing (..)
import BinaryHeap.Animation as Animation
import Types exposing (..)

make : BinaryHeap.Heap -> Model
make heap = Stable {
    heap = heap
  , animationState = Animation.initState 0
  }

update : DeltaTime -> Model -> Model
update dt prevModel = case prevModel of
  Stable model -> Stable { model |
      animationState = Animation.update dt model.animationState
    }
  Transition _ -> prevModel
