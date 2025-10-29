module Solutions.Group2 where

import Data.Array (filter)

import Prelude

-- 1. (Easy) Write a function `squared` which calculates the squares of an array
--           of numbers.
squared :: Array Number -> Array Number
squared = map (\x -> x*x)

-- 2. (Easy) Write a function `keepNonNegative` which removes the negative 
--           numbers from an array of numbers.
keepNonNegative :: Array Number -> Array Number
keepNonNegative = filter (_>=0.0)

-- 3. (Medium):
--     - Define an infix synonym `<$?>` for `filter`.
--     - write a `keepNonNegativeRewrite` function, which is the same as
--       `keepNonNegative`, but replaces `filter` with `<$?>`
--     - experiment with the precidence level and associativity of your operator
--       in PSCi.

infixl 4 filter as <$?>

keepNonNegativeRewrite :: Array Number -> Array Number
keepNonNegativeRewrite nums = (_>=0.0) <$?> nums
