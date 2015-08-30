module FP where

import List exposing ((::))  -- TODO: modify imports if you'd like
import List
import Result exposing (..)

digitsOfIntReverse : Int -> List Int
digitsOfIntReverse n = case n of
  0 -> []
  _ -> let last = n `rem` 10 in
       let initials = n // 10 in
         last::digitsOfIntReverse initials

digitsOfInt : Int -> List Int
digitsOfInt = List.reverse << digitsOfIntReverse

additivePersistence : Int -> Int
additivePersistence n =
  -- TODO
  0

digitalRoot : Int -> Int
digitalRoot n =
  -- TODO
  0

subsequences : List a -> List (List a)
subsequences xs =
  -- TODO
  []

take : Int -> List a -> Result String (List a)
take k xs =
  -- TODO
  Err "..."
