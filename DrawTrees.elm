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
  E.show tree
  -- E.spacer 0 0

signalTree : Signal Tree
signalTree =
  let trees = List.concatMap (balancedTrees 0) [1..20] in
  sampleListOn (Time.fps 16) trees

main : Signal E.Element
main =
  Signal.map2 view Window.dimensions signalTree
