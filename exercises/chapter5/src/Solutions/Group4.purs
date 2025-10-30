module Solutions.Group4 where

import Data.Foldable (foldl)

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

