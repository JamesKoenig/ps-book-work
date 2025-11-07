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

-- 2. (Medium) The `Action` class is a multi-parameter type class that defines
--             an action of one type on another:
class Monoid m <= Action m a where
  act :: m -> a -> a

-- laws:
--  act mempty a = a
--  act (m1 <> m2) = act m1 (act m2 a)

newtype Multiply = Multiply Int

instance Semigroup Multiply where
  append (Multiply n) (Multiply m) = Multiply (n * m)

instance Monoid Multiply where
  mempty = Multiply 1

instance Action Multiply Int where
  act (Multiply x) y = x*y
