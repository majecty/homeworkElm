module Input(inputEvent) where

import Signal
import Time
import Time exposing (Time)

import Types exposing (..)
import UI.Button as Button

fps : Signal Time
fps = Time.fps 30

type alias Frame = {
    dt : DeltaTime
  , insert : Bool
  }

makeFrame : DeltaTime -> Bool -> Frame
makeFrame dt insert = { dt = dt, insert = insert }

type ButtonOrTime = Button | Time

type alias RemainTurn = Int

buttonClickedPerFrame : Signal a -> Signal Bool
buttonClickedPerFrame buttonSignal =
  Signal.map fst <| buttonClickedPerFrameInternal buttonSignal

buttonClickedPerFrameInternal : Signal a -> Signal (Bool, RemainTurn)
buttonClickedPerFrameInternal buttonSignal =
  let button = Signal.map (\_ -> Button) buttonSignal
      time = Signal.map (\_ -> Time) fps
      merged = Signal.merge button time
      state = Signal.foldp
        (\event prev -> case (event, prev) of
          (Button, _) -> (True, 1)
          (Time, (_, 0)) -> (False, 0)
          (Time, (prevBool, remain)) -> (prevBool, remain - 1)
        )
        (False, 0)
        merged
  in
    Signal.sampleOn fps state

inputEvent : Signal Frame
inputEvent =
  let insert = buttonClickedPerFrame (.signal Button.message)
      merged = Signal.map2 makeFrame fps insert
  in
    Signal.sampleOn fps merged

