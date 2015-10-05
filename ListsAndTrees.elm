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

-- left tree has size n right tree has size (n + 1)
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
balancedTrees x n =
  balancedTrees2 x n |> fst

-- left trees is size n right trees is size (n + 1)
balancedTrees2 : Int -> Int -> (List Tree, List Tree)
balancedTrees2 x n =
  if
     | n <= 0 ->
       ([ Empty ], [ Node x Empty Empty ])

     | n `rem` 2 == 0 ->
       let subTreeSize = (n // 2) - 1 in
       let (subSmallerTrees, subGreaterTrees) = balancedTrees2 x subTreeSize in

       let smallerGreaters = mergeBalancedTrees x subSmallerTrees subGreaterTrees in
       let greaterSmallers = mergeBalancedTrees x subGreaterTrees subSmallerTrees in
       let smallerTrees = List.append smallerGreaters greaterSmallers in

       let greaterTrees = mergeBalancedTrees x subGreaterTrees subGreaterTrees in
       (smallerTrees, greaterTrees)

     | n `rem` 2 == 1 ->
       let subTreeSize = (n // 2) in
       let (subSmallerTrees, subGreaterTrees) = balancedTrees2 x subTreeSize in

       let smallerTrees = mergeBalancedTrees x subSmallerTrees subSmallerTrees in

       let smallerGreaters = mergeBalancedTrees x subSmallerTrees subGreaterTrees in
       let greaterSmallers = mergeBalancedTrees x subGreaterTrees subSmallerTrees in
       let greaterTrees = List.append smallerGreaters greaterSmallers in

       (smallerTrees, greaterTrees)

mergeBalancedTrees : Int -> List Tree -> List Tree -> List Tree
mergeBalancedTrees x leftTrees rightTrees =
  mapCartessian (\l r -> Node x l r) leftTrees rightTrees

mapCartessian : (a -> b -> c) -> List a -> List b -> List c
mapCartessian mapper listA listB =
  List.concatMap
    (\elementA ->
      List.map (\elementB -> mapper elementA elementB) listB
    )
    listA


completeTrees : Int -> Int -> List Tree
completeTrees _ _ =
  -- TODO
  []

almostCompleteTrees : Int -> Int -> List Tree
almostCompleteTrees _ _ =
  -- TODO
  []

