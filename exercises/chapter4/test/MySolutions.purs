module Test.MySolutions where

import Prelude

-- 1. (Easy) Write the `factorial` function using pattern matching.
--   Hint: Consider the two corner cases of zero and non-zero inputs.
--   Note: This is a repeat of an example from the previous chapter,
--         but see if you can rewrite it here on your own.

--based upon statements in the assignment we're going to assume we're not facing
-- negatives.  There's probably some world where you could type constraint it
-- using some really cool System F type shit.
factorial :: Int -> Int
factorial = factorial' 1
  -- tail-recursive version
  where factorial' :: Int -> Int -> Int
        factorial' acc 0 = acc
        factorial' acc n = factorial' (n*acc) (n-1)

-- 2. (Medium) [trunc.] Write a `binomial` function that produces the
--    coefficient for `x^k`-th term during the expansion of `(1+x)^n`

factFrac :: Int -> Int -> Int
factFrac = go 1
  where go :: Int -> Int -> Int -> Int
        go tail numer denom | numer == denom = tail
                            | otherwise      = go (tail*numer) (numer-1) denom

binomial :: Int -> Int -> Int
binomial 0 _ = 0
binomial _ 0 = 1
binomial n k | n < k     = 0
             | otherwise = binhelper n k (n-k)
             where binhelper :: Int -> Int -> Int -> Int
                   binhelper n k nk | k < nk    = binhelper n nk k
                                    | otherwise = div (factFrac n nk)
                                                      (factorial k)


