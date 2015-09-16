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

type alias Additive = { persistence: Int, root: Int }

chainAdditive : Additive -> Additive
chainAdditive additive  =
  if additive.root < 10
     then additive
     else chainAdditive { persistence = additive.persistence + 1, root = sumDigits additive.root }

additivePersistence : Int -> Int
additivePersistence n = .persistence <| chainAdditive { persistence = 0, root = n }

digitalRoot : Int -> Int
digitalRoot n = .root <| chainAdditive { persistence = 0, root = n }

sumList : List Int -> Int
sumList list = case list of
  [] -> 0
  x::xs -> x + (sumList xs)

sumDigits : Int -> Int
sumDigits = sumList << digitsOfInt

additivePersistenceHelper : Int -> Int -> Int
additivePersistenceHelper n previousCount =
  if n < 10
     then previousCount
     else additivePersistenceHelper (sumDigits n) (previousCount + 1)

subsequences : List a -> List (List a)
subsequences xs =
  -- TODO
  []

take : Int -> List a -> Result String (List a)
take k xs =
  -- TODO
  Err "..."
