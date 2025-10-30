module Solutions.Group4 where

import Data.Foldable (foldl)
import Data.Array    (cons)

import Prelude

-- 1. (Easy) Write a function `allTrue` which uses `foldl` to test whether an
--           array of boolean values are all true.

-- no short circuit :(
allTrue :: Array Boolean -> Boolean
allTrue = foldl (&&) true

-- 2. (Medium - No Test) Characterize those arrays `xs` for which the function
--                       `foldl (==) false xs` returns `true`.  In other words,
--                       complete the sentence: "The function returns `true`
--                       when `xs` contains..."
-- Answer: `xs :: Array Boolean` causes `foldl (==) false` to return true when
--         its head is false but then the rest of it is true e.g.:
--            `[false, true, true, true]`

-- 3. (Medium) Write a function `fibTailRec` which is the same as `fib` but in
--             tail recursive form.  _Hint_: use an accumulator parameter.

-- I had to whiteboard this for longer than I'm willing to admit
fibTailRec :: Int -> Int
fibTailRec n = fibh n 0 1
  --    rem  :: remaining steps
  --    acc  :: accumulator, or current fibbonacci value
  --    next :: next fibbonacci value
  where fibh :: Int -> Int -> Int -> Int
        fibh 0   acc _    = acc
        fibh rem acc next = fibh (rem-1) next (acc+next)

-- 4. (Medium) Write `reverse` in terms of `foldl`.
-- foldl :: forall (a:: Type) (b :: Type). (b -> a -> b) -> b -> Array a -> b
-- (flip cons) :: forall (a :: Type). Array a -> a -> Array a
reverse :: forall (a :: Type). Array a -> Array a
reverse = foldl (flip cons) []

