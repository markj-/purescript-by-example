module Main where

import Prelude
import Data.Array (filter, concatMap, (..))
import Data.Foldable (product)

infix 8 filter as <$?>

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

filterNegativeInfix :: Array Int -> Array Int
filterNegativeInfix xs = (\n -> n >= 0) <$?> xs

pairs n = concatMap (\i -> map (\j -> [i, j]) (i .. n)) (1 .. n)
factors n = filter (\pair -> product pair == n) (pairs n)
