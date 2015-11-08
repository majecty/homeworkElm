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
link (rank, Node x1 childTrees1 as tree1) (_, Node x2 childTrees2 as tree2) =
  if | x1 <= x2 -> (rank + 1, Node x1 (tree2::childTrees1))
     | otherwise -> (rank + 1, Node x2 (tree1::childTrees2))

insertTree : (Rank, Tree) -> InternalHeap -> InternalHeap
insertTree (rank, tree) internalHeap = case internalHeap of
  [] -> [(rank, tree)]

 -- Assume that inserted tree's rank is smaller than or euql to target heap.
 -- Because insertTree is only used in insert.
  (rank', tree')::ts' -> if
    | rank < rank' -> (rank, tree) :: internalHeap
    | rank == rank' -> insertTree (link (rank, tree) (rank', tree')) ts'

insert : Int -> Heap -> Heap
insert x (WrapHeap internalHeap) =
  WrapHeap <| insertTree (0, Node x []) internalHeap

merge_ : InternalHeap -> InternalHeap -> InternalHeap
merge_ rankTrees1 rankTrees2 = case (rankTrees1, rankTrees2) of
  ([], _) -> rankTrees2
  (_, []) -> rankTrees1
  ((r1, t1)::rankTrees1', (r2, t2)::rankTrees2') ->
    if | r1 < r2 -> (r1, t1) :: merge_ rankTrees1' rankTrees2
       | r1 > r2 -> (r2, t2) :: merge_ rankTrees2' rankTrees1
       | otherwise ->
          let newElem = (link (r1, t1) (r2, t2))
          in 
             insertTree newElem (merge_ rankTrees1' rankTrees2')

merge : Heap -> Heap -> Heap
merge (WrapHeap h1) (WrapHeap h2) =
  WrapHeap <| merge_ h1 h2

root : Tree -> Int
root (Node x _) = x

removeMinTree : InternalHeap -> ((Rank, Tree), InternalHeap)
removeMinTree ts = case ts of
  [t] -> (t, [])
  (r, t)::ts' ->
    let ((r', t'), ts'') = removeMinTree ts' in
    if | root t < root t' -> ((r, t), ts')
       | otherwise -> ((r', t'), (r, t)::ts'')

findMin : Heap -> Maybe Int
findMin (WrapHeap ts) = case ts of
  [] -> Nothing
  _ -> ts |> removeMinTree |> fst |> snd |> root |> Just

deleteMin : Heap -> Maybe Heap
deleteMin (WrapHeap ts) = case ts of
  [] -> Nothing
  _ -> ts |> removeMinTree |> snd |> WrapHeap |> Just
