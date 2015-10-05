module ListsAndTrees where

import List exposing ((::))
import List

suffixes xs = case xs of
  [] -> [[]]
  h::t -> xs :: suffixes t

type Tree = Empty | Node Int Tree Tree

mem : Int -> Tree -> Bool
mem _ _ =
  -- TODO
  False

fullTree : Int -> Int -> Tree
fullTree x h = case h of
  0 -> Empty
  _ -> Node x (fullTree x (h - 1)) (fullTree x (h - 1))

balancedTree : Int -> Int -> Tree
balancedTree x n =
  create2 x n |> fst

create2 : Int -> Int -> (Tree, Tree)
create2 x n =
  if
    | n <= 0 -> (Empty, Node x Empty Empty)
    | n `rem` 2 == 0 ->
      let subTreeN = (n // 2) - 1 in
      let (subSmallerTree, subGreaterTree) = create2 x subTreeN in
      let newSmallerTree = Node x subSmallerTree subGreaterTree in
      let newGreaterTree = Node x subGreaterTree subGreaterTree in
      (newSmallerTree, newGreaterTree)
    | n `rem` 2 == 1 ->
      let subTreeN = n // 2 in
      let (subSmallerTree, subGreaterTree) = create2 x subTreeN in
      let newSmallerTree = Node x subSmallerTree subSmallerTree in
      let newGreaterTree = Node x subSmallerTree subGreaterTree in
      (newSmallerTree, newGreaterTree)

balancedTrees : Int -> Int -> List Tree
balancedTrees _ _ =
  -- TODO
  []

completeTrees : Int -> Int -> List Tree
completeTrees _ _ =
  -- TODO
  []

almostCompleteTrees : Int -> Int -> List Tree
almostCompleteTrees _ _ =
  -- TODO
  []

