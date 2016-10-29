module FileOperations where

import Prelude
import Data.Path
import Data.Array (concatMap, (:))

allFiles :: Path -> Array Path
allFiles file = file : concatMap allFiles (ls file)

allFiles' :: Path -> Array Path
allFiles' file = file : do
  child <- ls file
  allFiles' child
