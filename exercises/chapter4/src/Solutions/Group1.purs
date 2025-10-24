module Solutions.Group1 where

import Prelude

-- ### Solutions for Chapter 4, Exercise group 1 ###

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
                   binhelper n' k' nk | k' < nk    = binhelper n' nk k'
                                      | otherwise  = div (factFrac n' nk)
                                                         (factorial k')

-- 3. (Medium) write a function `pascal` which uses Pascal's Rule for computing
--      the same binomial coefficients as the previous exercise.

-- for visual reference:   /--------------------------------------------------\
--    1       n=0          | pascal's theorem can be seen as an extension of: |
--   1 1      n=1          | (x+1)*(x^n+a_{n-1}*x^n-1+...) as x+1 distributes |
--  1 2 1     n=2          | over the rest of the polynomial, there are two   |
-- 1 3 3 1    n=3          | different ways to reach the same element degree, |
--1 4 6 4 1   n=4          | {x*(a*x^{n-1}) + 1*(b*x^n)}.                     |
--                         \--------------------------------------------------/
pascal :: Int -> Int -> Int
pascal _ 0 = 1
pascal 0 _ = 0
pascal n k | k > n     = 0
           | n-k < k   = pascal n (n-k)  -- the triangle is symmetric.
           | otherwise = (pascal (n-1) k) + (pascal (n-1) (k-1))
