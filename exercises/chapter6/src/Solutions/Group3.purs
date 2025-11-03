module Solutions.Group3 where

import Prelude

import Data.Generic.Rep  (class Generic)
import Data.Show.Generic (genericShow)

data NonEmpty a = NonEmpty a (Array a)

-- the tests won't work if we don't define a Show instance, for whatever reason
derive instance genericNonEmpty :: Generic (NonEmpty a) _
instance nonEmptyShow :: Show a => Show (NonEmpty a) where
  show = genericShow

-- 1. (Easy) Write an `Eq` instance for the type `NonEmpty a` that reüses
--           instances for `Eq a` and `Eq (Array a)`

-- prelude has an Eq a => Eq (Array a) instance already acc. to the book
derive instance nonEmptyEq :: Eq a => Eq (NonEmpty a)
----manual version:
--instance nonEmptyEq :: Eq a => Eq (NonEmpty a) where
--  eq (NonEmpty x xs) (NonEmpty y ys) = x == y && xs == ys

