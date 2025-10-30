module Solutions.Group4 where

import Data.Foldable (foldl)

import Prelude

allTrue :: Array Boolean -> Boolean
allTrue = foldl (&&) true
