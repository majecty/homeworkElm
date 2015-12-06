module BinaryHeap.Model where

import BinaryHeap.BinaryHeap as BinaryHeap
import BinaryHeap.Types exposing (..)
import BinaryHeap.Animation as Animation

make : BinaryHeap.Heap -> Model
make heap = Stable {
    heap = heap
  , animationState = Animation.initState 0
  }
