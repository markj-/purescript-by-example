module Main where

import Prelude
import Data.Array (filter, concatMap, (..), length)
import Data.Foldable (product)
import Control.MonadZero (guard)

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

factors' :: Int -> Array (Array Int)
factors' n = do
  i <- 1 .. n
  j <- i .. n
  guard $ i * j == n
  pure [i, j]

isPrime :: Int -> Boolean
isPrime n =
  if n > 1
    then length (factors n) == 1
    else false

cartesian :: Array Int -> Array Int -> Array (Array Int)
cartesian xs ys = do
  i <- xs
  j <- ys
  pure [i, j]

triples :: Int -> Array (Array Int)
triples n = do
  let x = n - 1
  a <- 1 .. x
  b <- 1 .. x
  c <- 1 .. x
  guard $ (a * a) + (b * b) == (c * c)
  pure [a, b, c]
