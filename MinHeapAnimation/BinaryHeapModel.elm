module BinaryHeapModel where

import BinaryHeap
import BinaryHeapTypes exposing (..)
import BinaryHeapAnimation

make : BinaryHeap.Heap -> Model
make heap = Stable {
    heap = heap
  , animationState = BinaryHeapAnimation.initState 0
  }
