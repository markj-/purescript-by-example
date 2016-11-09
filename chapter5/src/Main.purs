module Main where

import Prelude
import Data.Picture

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

type Address = { street :: String, city :: String }

type Person = { name :: String, address :: Address }

sameCity :: Person -> Person -> Boolean
sameCity { address: { city: x } } { address: { city: y } } = x == y

fromSingleton :: forall a. a -> Array a -> a
fromSingleton def [x] = x
fromSingleton def _ = def

origin :: Point
origin = Point { x, y }
  where
    x = 0.0
    y = 0.0

circleRadiusTen :: Shape
circleRadiusTen = Circle origin 10.0
