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

-- 2. (Medium) Write a function `cartesianProduct` which uses do notation to
--             find the _cartesian product_ of two arrays, i.e. the set of all
--             pairs of elements `a`,`b`, where `a` is an element of the first
--             array and `b` is an element of the second.

-- having messed with purescript and haskell outside of this book definitely
-- gives me an edge in understanding this stuff....

-- if the expected output was Array (Tuple a b) this could be
-- :: forall a b. Array a -> Array b -> Array (Tuple a b)
-- but the test exercises expects Array (Array a)
cartesianProduct :: forall a. Array a -> Array a -> Array (Array a)
cartesianProduct as bs = do
  a <- as
  b <- bs
  pure $ [a,b]
