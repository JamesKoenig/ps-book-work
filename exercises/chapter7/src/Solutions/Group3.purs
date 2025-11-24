module Solutions.Group3 where

import Prelude
import Data.Generic.Rep  (class Generic)
import Data.Show.Generic (genericShow)
import Data.Foldable     (class Foldable
                         ,foldr
                         ,foldl
                         ,foldMap
                         ,foldlDefault
                         ,foldrDefault
                         )

-- 1. (Easy) Write an `Eq` and `Show` instance for the following binary tree
--           data structure
data Tree a = Leaf | Branch (Tree a) a (Tree a)

derive instance eqTree :: Eq a => Eq (Tree a)
derive instance genericTree :: Generic (Tree a) _
-- this can't be a point free version for some reason
instance showTree :: Show a => Show (Tree a) where
  show tree = genericShow tree

-- 2. (Medium) Write a `Traversable` instance for `Tree a`, which combines
--             side-effects left-to-right

-- Traverasable requires Functor and Foldable constraints
instance Functor Tree where
  map f Leaf = Leaf
  map f (Branch left val right) = Branch (f <$> left) (f val) (f <$> right)

-- we did this before in Ch6 Grp3 Ex5 & Ex6-- that was hard
-- the instance fns for foldable are:
--  foldr   :: forall a b. (a -> b -> b) -> b -> f a -> b
--  foldl   :: forall a b. (b -> a -> b) -> b -> f a -> b
--  foldMap :: forall a m. Monoid m => (a -> m) -> f a -> m
instance Foldable Tree where
-- get the easiest one oout of the way
  foldMap fm Leaf = mempty
  foldMap fm (Branch left x right) =
    (foldMap fm left) <> (fm x) <> (foldMap fm right)

  -- we can do manual versions of this later, technically foldMap+defaults are
  -- sufficient to define folds for now
  foldl f = foldlDefault f
  foldr f = foldrDefault f
