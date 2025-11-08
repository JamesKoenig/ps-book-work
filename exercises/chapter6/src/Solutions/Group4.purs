module Solutions.Group4 where

import Prelude

import Data.Maybe    (Maybe
                     ,fromJust
                     )
import Data.Foldable (maximum)
import Data.Monoid   (power
                     )
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

-- 4. (Medium) Write an `Action` instance that repeats an input string some
--             number of times
-- Data.Monoid.power :: forall (m :: Type). Monoid m => m -> Int -> a
instance Action Multiply String where
  act (Multiply x) y = power y x

-- 5. (Medium) Write an instance `Action m a => Action m (Array a), where the
--             action on arrays is defined by acting on each array element
--             independently

instance Action m a => Action m (Array a) where
  act m fx = (act m) <$> fx

--instance (Functor f, Action m a) => Action m (f a) where
--  act m fx = (act m) <$> fx

-- 6. (Difficult) Given the following newtype, write an instance for
--                `Action m (Self m)` where the monoid acts on itself using
--                `append`
newtype Self m = Self m

derive newtype instance Eq a => Eq (Self a)
derive newtype instance Show a => Show (Self a)
derive newtype instance Eq Multiply
derive newtype instance Show Multiply

instance Monoid m => Action m (Self m) where
  act m (Self n) = Self $ m <> n
