module Solutions.Group3 where

import Prelude

import Test.Examples (factorsV3)
--import Data.Int    as Int
--import Data.Number as Num

-- 1. (Easy) Write a function `isPrime`, which tests whether its integer
--           argument is prime. _Hint_: Use the `factors` function.

-- TODO: once we're done with the foreign function interface part of the book
--       reïmplement this with a sieve of Eratosthenes.
isPrime :: Int -> Boolean
isPrime n | n < 2     = false
          | otherwise = factorsV3 n == [[1,n]]
