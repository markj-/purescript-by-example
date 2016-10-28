module Main where

import Prelude

isEven :: Int -> Boolean
isEven x =
  if x < 0
    then false
    else if x == 0
      then true
      else isEven (x - 2)
