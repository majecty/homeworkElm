module BinomialHeaps
  (Heap, empty, isEmpty, findMin, insert, deleteMin, merge) where

import List exposing ((::))
import List

type alias Rank = Int
type Tree = Node Rank Int (List Tree)

type alias InternalHeap = List Tree
type Heap = WrapHeap InternalHeap

rank (Node r _ _) = r
root (Node _ x _) = x

-- pre-condition: rank(t1) = rank(t2)
--
link : Tree -> Tree -> Tree
link t1 t2 =
  let (Node r x1 c1) = t1
      (Node _ x2 c2) = t2
  in
    if | x1 <= x2  -> Node (1+r) x1 (t2::c1)
       | otherwise -> Node (1+r) x2 (t1::c2)

insertTree : Tree -> InternalHeap -> InternalHeap
insertTree t ts = case ts of
  []      -> [t]
  t'::ts' -> if
    | rank t == rank t' -> insertTree (link t t') ts'
    | rank t <  rank t' -> t :: ts

-- merge_ ts1 ts2 = ts3
--
--   pre:  forall 0 <= i < len(ts1)-1. rank(ts1[i]) < rank(ts1[i+1]) &&
--         forall 0 <= i < len(ts2)-1. rank(ts2[i]) < rank(ts2[i+1])
--
--   post: forall 0 <= i < len(ts3)-1. rank(ts3[i]) < rank(ts3[i+1]) &&
--         len(ts3) <= 1 + max (len ts1) (len ts2)
--
merge_ : InternalHeap -> InternalHeap -> InternalHeap
merge_ ts1 ts2 = case (ts1, ts2) of
  ([], _) -> ts2
  (_, []) -> ts1
  (t1::ts1', t2::ts2') ->
    if | rank t1 < rank t2 -> t1 :: merge_ ts1' ts2
       | rank t2 < rank t1 -> t2 :: merge_ ts2' ts1
       | otherwise         -> insertTree (link t1 t2) (merge_ ts1' ts2')

removeMinTree : InternalHeap -> (Tree, InternalHeap)
removeMinTree ts = case ts of
  [t]    -> (t, [])
  t::ts' ->
    let (t',ts'') = removeMinTree ts' in
    if | root t < root t' -> (t, ts')
       | otherwise        -> (t', t::ts'')

empty : Heap
empty = WrapHeap []

isEmpty : Heap -> Bool
isEmpty h = h == empty

insert : Int -> Heap -> Heap
insert x (WrapHeap ts) = WrapHeap (insertTree (Node 0 x []) ts)

merge : Heap -> Heap -> Heap
merge (WrapHeap ts1) (WrapHeap ts2) = WrapHeap (merge_ ts1 ts2)

findMin0 : Heap -> Maybe Int
findMin0 (WrapHeap ts) = case ts of
  [] -> Nothing
  _  -> ts |> List.map root |> List.foldl min 2147483647 |> Just

findMin : Heap -> Maybe Int
findMin (WrapHeap ts) = case ts of
  [] -> Nothing
  _  -> ts |> removeMinTree |> fst |> root |> Just

deleteMin : Heap -> Maybe Heap
deleteMin (WrapHeap ts) = case ts of
  [] -> Nothing
  _  -> let (Node _ x ts1, ts2) = removeMinTree ts in
        Just <| WrapHeap <| merge_ (List.reverse ts1) ts2


-- Alternative definition of merge_, adapted from:
--
-- http://stackoverflow.com/questions/11462626/
--   should-melding-merging-of-binomial-heaps-be-done-in-one-pass-or-two

merge' : InternalHeap -> InternalHeap -> InternalHeap
merge' ts1 ts2 = case (ts1, ts2) of
  ([], _) -> ts2
  (_, []) -> ts1
  (t1::ts1', t2::ts2') ->
    if | rank t1 < rank t2 -> t1 :: merge' ts1' ts2
       | rank t2 < rank t1 -> t2 :: merge' ts2' ts2
       | otherwise         -> merge_wc (link t1 t2) ts1' ts2'

merge_wc : Tree -> InternalHeap -> InternalHeap -> InternalHeap
merge_wc t ts1 ts2 = case (ts1, ts2) of
  ([], _) -> insertTree t ts2
  (_, []) -> insertTree t ts1
  (t1::ts1', t2::ts2') ->
    let (r,r1,r2) = (rank t, rank t1, rank t2) in
    if | r <  r1 && r <  r2 -> t :: merge' ts1 ts2
       | r <  r1 && r == r2 -> merge_wc (link t t2) ts1 ts2'
       | r == r1 && r <  r2 -> merge_wc (link t t1) ts1' ts2
       | r == r1 && r == r2 -> merge_wc (link t t1) ts1' ts2

