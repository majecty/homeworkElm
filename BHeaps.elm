module BHeaps
  (Heap, empty, isEmpty, findMin, insert, deleteMin, merge) where

import List exposing ((::))
import List

type Tree = Node Int (List Tree)
type alias Rank = Int
type alias InternalHeap = List (Rank, Tree)
type Heap = WrapHeap InternalHeap

{-- Internal Helpers ----------------------------------------------------}

-- TODO

{-- External Interface --------------------------------------------------}

empty : Heap
empty = WrapHeap []

isEmpty : Heap -> Bool
isEmpty h = h == empty

link : (Rank, Tree) -> (Rank, Tree) -> (Rank, Tree)
link (r, Node x1 ts1 as t1) (_, Node x2 ts2 as t2) =
  if | x1 <= x2 -> (r + 1, Node x1 (t2::ts1))
     | otherwise -> (r + 1, Node x2 (t1::ts2))

insertTree : (Rank, Tree) -> InternalHeap -> InternalHeap
insertTree (rank, tree) internalHeap = case internalHeap of
  [] -> [(rank, tree)]

  (rank', tree')::ts' -> if
    | rank < rank' -> (rank, tree) :: internalHeap
    | rank == rank' -> insertTree (link (rank, tree) (rank', tree')) ts'

insert : Int -> Heap -> Heap
insert x (WrapHeap internalHeap) =
  WrapHeap <| insertTree (0, Node x []) internalHeap

merge_ : InternalHeap -> InternalHeap -> InternalHeap
merge_ ts1 ts2 = case (ts1, ts2) of
  ([], _) -> ts2
  (_, []) -> ts1
  ((r1, t1)::ts1', (r2, t2)::ts2') ->
    if | r1 < r2 -> (r1, t1) :: merge_ ts1' ts2
       | r1 > r2 -> (r2, t2) :: merge_ ts2' ts1
       | otherwise ->
          let newElem = (r1, (link t1 t2))
          in
           insertTree newElem (merge_ ts1' ts2')

merge : Heap -> Heap -> Heap
merge (WrapHeap h1) (WrapHeap h2) =
  WrapHeap <| merge_ h1 h2

findMin : Heap -> Maybe Int
findMin h =
  -- TODO
  Nothing

deleteMin : Heap -> Maybe Heap
deleteMin h =
  -- TODO
  Nothing

