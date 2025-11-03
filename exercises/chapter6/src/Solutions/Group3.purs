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

-- 2. (Meidum) Write a `Semigroup` instance for `NonEmpty a` by reusing the
--             `Semigroup` instance for `Array`
instance nonEmptySemigroup :: Semigroup (NonEmpty a) where
  append (NonEmpty x xs) (NonEmpty y ys) = NonEmpty x $ xs <> pure y <> ys

-- 3. (Medium) Write a `Functor` instance for `NonEmpty`

-- it helps that I did this a TON during the functor chapter of haskell from
--  first principles.  Though in this case it's `map` instead of `fmap`
instance nonEmptyFunctor :: Functor NonEmpty where
  map f (NonEmpty x xs) = (NonEmpty (f x) (map f xs))
