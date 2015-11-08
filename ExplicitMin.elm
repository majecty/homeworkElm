module ExplicitMin where
  -- (Heap, empty, isEmpty, findMin, insert, deleteMin, merge) where

-- NOTE: without functors or type classes, we would manually swap in
-- different implementations of H by twiddling the following imports

import BinomialHeaps as H
-- import LeftistHeaps as H

type Heap a =
       E
     | NE Int a   -- the Int is the minimum element

type alias HeapFunctions a = {
    empty : a,
    isEmpty : a -> Bool,
    insert : Int -> a -> a,
    merge : a -> a -> a,
    findMin : a -> Maybe Int,
    deleteMin : a -> Maybe a
  }

binominalFunctions = {
    empty = H.empty,
    isEmpty = H.isEmpty,
    insert = H.insert,
    merge = H.merge,
    findMin = H.findMin,
    deleteMin = H.deleteMin
  }

wrap : HeapFunctions a -> HeapFunctions (Heap a)
wrap fs = {
    empty = empty fs,
    isEmpty = isEmpty fs,
    insert = insert fs,
    merge = merge fs,
    findMin = findMin fs,
    deleteMin = deleteMin fs
  }

empty : HeapFunctions a -> Heap a
empty fs = E

isEmpty : HeapFunctions a -> Heap a -> Bool
isEmpty fs heap = case heap of
  E -> True
  _ -> False

insert : HeapFunctions a -> Int -> Heap a -> Heap a
insert fs x heap = case heap of
  E -> NE x <| fs.insert x fs.empty

  NE previousMin internalHeap ->
    let newMin = min previousMin x
    in
       NE newMin <| fs.insert x internalHeap

merge : HeapFunctions a -> Heap a -> Heap a -> Heap a
merge fs heap1 heap2 = case (heap1, heap2) of
  (E, _) -> heap2
  (_, E) -> heap1

  (NE min1 internalHeap1, NE min2 internalHeap2) ->
    NE (min min1 min2) (fs.merge internalHeap1 internalHeap2)

findMin : HeapFunctions a -> Heap a -> Maybe Int
findMin fs heap = case heap of
  E -> Nothing
  NE min _ -> Just min

deleteMin : HeapFunctions a -> Heap a -> Maybe (Heap a)
deleteMin fs heap = case heap of
  E -> Nothing
  NE _ internalHeap ->
    let maybeNewHeap = fs.deleteMin internalHeap
        maybeNewMin = Maybe.andThen maybeNewHeap fs.findMin
    in
       case (maybeNewHeap, maybeNewMin) of
         (Just newHeap, Just newMin) -> NE newMin newHeap |> Just
         _ -> Nothing
