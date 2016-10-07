module Main where

import Control.Monad.Eff.Console

import Math (sqrt, pi)

import Prelude

diagonal w h = sqrt(w * w + h * h)

circleArea r = pi * r * r

-- main = logShow (diagonal 3.0 4.0)

-- main = logShow (circleArea 3.0)

