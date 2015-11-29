module BinaryHeapViewer(view) where

import Array
import Graphics.Collage as Collage
import Text

import BinaryHeap

emptyForm : Collage.Form
emptyForm = Collage.text <| Text.fromString "Empty"

view : BinaryHeap.Heap -> Collage.Form
view heap = 
  let internalArray = BinaryHeap.getInternalArray heap
      internalList = Array.toList internalArray
  in
     emptyForm
