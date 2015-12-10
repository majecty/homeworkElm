module BinaryHeap.Model where

import BinaryHeap.BinaryHeap as BinaryHeap
import BinaryHeap.Types exposing (..)
import BinaryHeap.Animation as Animation
import Debug
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

insert : Int -> Model -> Model
insert element prevModel = case prevModel of
    Stable model -> Stable { model | 
        heap = BinaryHeap.insert element model.heap
      }
    Transition _ -> Debug.crash "Invalid operation."

makeHeap : List Int -> Model
makeHeap xs = make <| makeHeapInternal xs

makeHeapInternal : List Int -> BinaryHeap.Heap
makeHeapInternal xs = case xs of
  [] -> BinaryHeap.empty
  x::xs' -> BinaryHeap.insert x <| makeHeapInternal xs'
