module Solutions.Group4 where

import Prelude

import Data.Maybe    (Maybe
                     ,fromJust
                     )
import Data.Foldable (maximum)
-- 1. (Medium) Define a partial function:
--                `unsafeMaximum :: Partial => Array Int -> Int`
--             that finds the maximum of a non-empty array of integers.  Test
--             out your function in PSCi using `unsafePartial`.
--             *Hint*: Use the `maximum` function from `Data.Foldable`
unsafeMaximum :: Partial => Array Int -> Int
unsafeMaximum = fromJust <<< (maximum :: Array Int -> Maybe Int)

