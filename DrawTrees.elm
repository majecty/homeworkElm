module DrawTrees where

import List exposing ((::))
import List
import Maybe
import ListsAndTrees exposing (..)

import Color
import Signal
import Window
import Mouse
import Text as T
import Time
import Graphics.Element as E
import Graphics.Collage as C

type alias SampleListState a = { original: List a, remain: List a }

getNextElement : SampleListState a -> (Maybe a, SampleListState a )
getNextElement state = case state.remain of
  [] ->
    let firstOfOriginal = List.head state.original in
    let newState = { state | remain <- state.original } in
      (firstOfOriginal, newState)
  _ ->
    let firstOfOriginal = List.head state.remain in
    let newRemain = Maybe.withDefault [] <| List.tail state.remain in
    let newState = { state | remain <- newRemain } in
      (firstOfOriginal, newState)

sampleState : Signal b -> List a -> Signal (Maybe a, SampleListState a)
sampleState source list =
  let initialState = getNextElement { original = list, remain = list } in
  Signal.foldp (\_ (_, prevState) -> getNextElement prevState) initialState source

sampleListOn : Signal b -> List a -> Signal a
sampleListOn source xs =
  let valueAndState = sampleState source xs in
  Signal.map (\(Just value, state) -> value) valueAndState

view : (Int,Int) -> Tree -> E.Element
view (w, h) tree =
  let (_, allTreeForm) = tree2Form tree in
  C.collage w h [ allTreeForm ]
  -- E.show tree
  -- E.spacer 0 0

tree2Form : Tree -> (Int, C.Form)
tree2Form tree =
  case tree of
    Empty -> (1, circle)

    Node _ leftTree rightTree ->
      let (leftLength, leftTreeForm) = tree2Form leftTree in
      let (rightLength, rightTreeForm) = tree2Form rightTree in
      let length = 2 * (max leftLength rightLength) + 2 in
      let lengthf = toFloat length in
      let left = C.move (-5 * (lengthf / 2), -20) leftTreeForm in
      let right = C.move (5 * (lengthf / 2), -20) rightTreeForm in
      (length, C.group [ left, right, circle ])

circle : C.Form
circle = C.filled Color.black <| C.circle 5

signalTree : Signal Tree
signalTree =
  let trees = List.concatMap (balancedTrees 0) [1..20] in
--  sampleListOn (Mouse.clicks) trees
  sampleListOn (Time.fps 30) trees

main : Signal E.Element
main =
  Signal.map2 view Window.dimensions signalTree
