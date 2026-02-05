module Solutions.Group2 where

import Prelude

import Effect (Effect)
import Effect.Exception (error, throwException)

-- 1. (Medium) Rewrite the `safeDivide` function as exceptionDivide and throw an
--             exception using `throwException` with the message "div zero" if
--             the denominator is zero.
exceptionDivide :: Int -> Int -> Effect Int
exceptionDivide x y
  | y == 0    = throwException $ error "div zero"
  | otherwise = pure (x `div` y)

-- 2. (Medium) Write a function `estimatePi :: Int -> Number` that uses `m` in
--             terms of the Gregory Series to calculate an approximation of 
--             `pi`.
--estimatePi :: Int -> Number

-- 3. (Medium) Write a function `fibonacci :: Int -> Int` to compute the `n`th
--             Fibonacci number, using `ST` to track the values of the previous
--             two Fibonacci numbers.  Using PSCi, compare the speed of your new
--             ST-based implementation against the recursive implementation
--             (`fib`) from Chapter 5.

--the recursive implementation from chapter 5:
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
