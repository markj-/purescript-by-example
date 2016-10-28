module Main where

import Prelude
import Data.Array (filter)

isEven :: Int -> Boolean
isEven x =
  if x < 0
    then false
    else if x == 0
      then true
      else isEven (x - 2)

square :: Array Int -> Array Int
square = map (\n -> n * n)

filterNegative :: Array Int -> Array Int
filterNegative = filter (\n -> n >= 0)
