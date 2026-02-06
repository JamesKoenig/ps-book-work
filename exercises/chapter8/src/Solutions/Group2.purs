module Solutions.Group2 where

import Prelude

import Effect (Effect)
import Effect.Exception (error, throwException)
import Control.Monad.ST.Ref (modify
                            ,new
                            ,read
                            )
import Control.Monad.ST (for
                        ,run
                        )
import Data.Int (toNumber)

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

-- in latex:
-- \pi=\lim_{n\to\inf} \left[ 4\cdot\sum_{k=1}^n \frac{(-1)^{k+1}}{2k-1} \right]
-- in pseudocode:
-- lim(n->inf) 4*SUM(from: 1, to: n, (odd(k) ? 1 : -1)/(2*k-1))
estimatePi :: Int -> Number
estimatePi n = run do
  ref <- new 0.0
  for 1 n \k ->
    modify (step k) ref

  final <- read ref
  pure $ 4.0*final

  where step :: Int -> Number -> Number
        step k x =
          let sign  = if (k `mod` 2) == 1
                      then  1.0
                      else -1.0
              denom = 2.0*(toNumber k)-1.0
              frac  = sign/denom
          in x + frac

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
