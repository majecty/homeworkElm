module BinaryHeapViewer(view) where

import Array
import Graphics.Collage as Collage
import Maybe
import Text

import BinaryHeap
import Pos exposing (makePos)
import Types exposing (..)

emptyForm : Collage.Form
emptyForm = Collage.text <| Text.fromString "Empty"

elements : BinaryHeap.Heap -> List Int
elements heap =
  let internalArray = BinaryHeap.getInternalArray heap
      internalList = Array.toList internalArray
  in
    internalList

view : BinaryHeap.Heap -> Collage.Form
view heap =
  let elems = elements heap
      length = List.length elems
  in
     case elems of
       [] -> emptyForm
       _ ->
         let floors = calcPoses length 1
             allNodes = List.map .allNodes floors |> List.concat
             allForms = List.map2 showElement elems allNodes
         in
            Collage.group allForms

showElement : Int -> Pos -> Collage.Form
showElement value position =
  Collage.move (position.x, position.y) <| Collage.text <| Text.fromString <| toString value

{-|
  floor : tree elements which has same depth
  depth : distances from root + 1
          root has depth 1
  width : length of floor
  gap   : gap between same floor nodes when printed
  minimumGap : last floor's gap
               although floor has just 1 element, it has the minimum gap.
  height : difference of y position between 2 floors.
           this value is always same in tree.
-}

type alias NumOfElement = Int
type alias DepthFromRoot = Int
type alias Gap = Float
type alias Height = Float
type alias Floor = {
    firstNode : Pos
  , allNodes : List Pos
  }

type alias Width = Int

type alias List1 a = {
    head : a
  , all : List a
  }

makeFloor : Pos -> List Pos -> Floor
makeFloor firstPos poses = { firstNode = firstPos, allNodes = poses }

-- depth is start from 1
-- depth  width
--     1      1
--     2      2
--     3      4
widthInRoot : DepthFromRoot -> Width
widthInRoot d = 2 ^ (d - 1)

-- quest : don't use this.
unsafe : Maybe a -> a
unsafe maybeA = case maybeA of
  Just a -> a
  Nothing -> Debug.crash "Impossible get from empty."

minimumGap : Gap
minimumGap = 20 -- means 20 pixel

height : Height
height = 20

gapFromFloor : Floor -> Gap
gapFromFloor {allNodes} = case allNodes of
  [] -> minimumGap
  [e] -> minimumGap
  e1 :: e2 :: tail -> e2.x - e1.x

unFold : Width -> a -> (a -> a) -> List1 a
unFold length defaultValue nextGenerator =
  if length <= 0 then
    Debug.crash "Unfold need more than 1 number."
  else if length == 1 then
    { head = defaultValue, all = [defaultValue] }
  else
    let nextValue = nextGenerator defaultValue
        nextLength = length - 1
        subList = unFold nextLength nextValue nextGenerator
    in
      { head = defaultValue
      , all = defaultValue :: (subList.all)
      }

calcPoses : NumOfElement -> DepthFromRoot -> List Floor
calcPoses num d =
  if num <= 0
     then []
     else
      let width = widthInRoot d
          subResult =
            let leftNum = max 0 (num - width) in
               calcPoses leftNum (d + 1)
          maybeBelowFloor = List.head subResult
          gap = case maybeBelowFloor of
            Just belowFloor ->
              let belowGap = gapFromFloor belowFloor
              in
                 belowGap * 2
            Nothing -> minimumGap
          startX = case maybeBelowFloor of
            Just belowFloor ->
              let startOfBelowFloor = belowFloor |> .firstNode |> .x
                  belowGap = gapFromFloor belowFloor
              in
                 startOfBelowFloor + (belowGap / 2)
            Nothing -> 0
          xs = unFold width startX (\prevX -> prevX + gap)
          y = case maybeBelowFloor of
            Just belowFloor ->
              let yOfBelowFloor = belowFloor |> .firstNode |> .y
              in
                 yOfBelowFloor + height
            Nothing -> 0
      in
         let allNodes = (List.map (\x -> makePos x y) xs.all)
             firstNode = makePos (xs.head) y
         in
            (makeFloor firstNode allNodes) :: subResult
