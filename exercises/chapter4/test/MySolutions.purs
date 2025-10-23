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


