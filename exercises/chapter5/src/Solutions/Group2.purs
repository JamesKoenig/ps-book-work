module Solutions.Group2 where

import Prelude

-- 1. (Easy) Write a function `squared` which calculates the squares of an array
--           of numbers.
squared :: Array Number -> Array Number
squared = map (\x -> x*x)
