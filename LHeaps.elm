module LHeaps where

import List exposing ((::))
import List

{-- Copied from LeftistHeaps.elm from class. DO NOT MODIFY. -------------}

type alias Rank = Int
type Heap = E | T Rank Int Heap Heap

rank : Heap -> Rank
rank h = case h of {E -> 0; T r _ _ _ -> r}

makeT : Int -> Heap -> Heap -> Heap
makeT x h1 h2 =
  let (l,r) = if | rank h1 >= rank h2 -> (h1, h2)
                 | otherwise          -> (h2, h1) in
  T (1 + rank r) x l r

merge : Heap -> Heap -> Heap
merge h1 h2 = case (h1, h2) of
  (_, E) -> h1
  (E, _) -> h2 
  (T _ x1 l1 r1, T _ x2 l2 r2) ->
    if | x1 <= x2  -> makeT x1 l1 (merge r1 h2)
       | otherwise -> makeT x2 l2 (merge h1 r2)

{------------------------------------------------------------------------}

-- This algorithm Theta (leftest spine height of heap)
-- In worst case, all data of heap is in leftest spine, it always Thenta(n)
insert : Int -> Heap -> Heap
insert value heap = case heap of
  E -> T 1 value E E

  (T rank rootValue leftHeap rightHeap) ->
    if
      | rootValue < value ->
        let newLeftHeap = (insert value leftHeap)
        in
          (T rank rootValue newLeftHeap rightHeap)
      | otherwise ->
        let newLeftHeap = (insert rootValue leftHeap)
        in
           (T rank value newLeftHeap rightHeap)

mergePairs : List Heap -> List Heap
mergePairs listOfHeap = case listOfHeap of
  fstHeap::secondHeap::lefts -> (merge fstHeap secondHeap)::(mergePairs lefts)

  _ -> listOfHeap

makePass : List Heap -> List Heap
makePass listOfHeap = case listOfHeap of
  [] -> []
  onlyHeap::[] -> listOfHeap

  _ -> makePass <| mergePairs listOfHeap

-- S(n,m) is the time that mergePairs hs takes, where n is the length of hs and m is the size of the largest heap in hs
-- T(n,m) is the time that makePass hs takes, where n is the length of hs and m is the size of the largest heap in hs.
-- worst case, 
-- mergePair -> r | r * n + r * (1/2)n ... + r
-- if n = 2^k | k * r * (1 + 1/2 + ... 1 / 2^k) = k * r * (1 - 1 / n) = r * (log n) * (1 - 1/n)

-- r_i = S(n, m) <= (log m) * (n / 2)
-- r_i+1 = S(n/2, 2m) <= (log 2m) * (n / 4)
-- worst case n * m = n_0
-- (log 1) * (n) + (log 2) * (n/2) + (log 2^2) * (n / 2^2) + ... + 1 * log m
-- n * (1/2 + 2/2^2 + 3/2^3 + .. k/n) * (log 2)
fromList : List Int -> Heap
fromList listOfInt =
  let initialHeapList = List.map (\x -> T 1 x E E) listOfInt
  in
     Maybe.withDefault E <| List.head <| makePass initialHeapList
