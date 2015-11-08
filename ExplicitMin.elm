module ExplicitMin
  (Heap, empty, isEmpty, findMin, insert, deleteMin, merge) where

-- NOTE: without functors or type classes, we would manually swap in
-- different implementations of H by twiddling the following imports

import BinomialHeaps as H
-- import LeftistHeaps as H

type Heap =
       E
     | NE Int H.Heap   -- the Int is the minimum element

empty : Heap
empty = E

isEmpty : Heap -> Bool
isEmpty heap = case heap of
  E -> True
  _ -> False

insert : Int -> Heap -> Heap
insert x heap = case heap of
  E -> NE x <| H.insert x H.empty

  NE previousMin internalHeap ->
    let newMin = min previousMin x
    in
       NE newMin <| H.insert x internalHeap

merge : Heap -> Heap -> Heap
merge heap1 heap2 = case (heap1, heap2) of
  (E, _) -> heap2
  (_, E) -> heap1

  (NE min1 internalHeap1, NE min2 internalHeap2) ->
    NE (min min1 min2) (H.merge internalHeap1 internalHeap2)

findMin : Heap -> Maybe Int
findMin heap = case heap of
  E -> Nothing
  NE min _ -> Just min

deleteMin : Heap -> Maybe Heap
deleteMin heap = case heap of
  E -> Nothing
  NE _ internalHeap ->
    let maybeNewHeap = H.deleteMin internalHeap
        maybeNewMin = Maybe.andThen maybeNewHeap H.findMin
    in
       case (maybeNewHeap, maybeNewMin) of
         (Just newHeap, Just newMin) -> NE newMin newHeap |> Just
         _ -> Nothing
