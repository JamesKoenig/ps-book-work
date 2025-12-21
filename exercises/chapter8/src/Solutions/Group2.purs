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
