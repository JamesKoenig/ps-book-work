module Solutions.Group3 where

import Prelude
import Data.Generic.Rep  (class Generic)
import Data.Show.Generic (genericShow)
import Data.Foldable     (class Foldable
                         ,foldMap
                         ,foldlDefault
                         ,foldrDefault
                         )
import Data.Traversable (class Traversable
                        ,traverse
                        ,sequenceDefault
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
  map _ Leaf = Leaf
  map f (Branch left val right) = Branch (f <$> left) (f val) (f <$> right)

-- we did this before in Ch6 Grp3 Ex5 & Ex6-- that was hard
-- the instance fns for foldable are:
--  foldr   :: forall a b. (a -> b -> b) -> b -> f a -> b
--  foldl   :: forall a b. (b -> a -> b) -> b -> f a -> b
--  foldMap :: forall a m. Monoid m => (a -> m) -> f a -> m
instance Foldable Tree where
-- get the easiest one oout of the way
  foldMap _  Leaf = mempty
  foldMap fm (Branch left x right) =
    (foldMap fm left) <> (fm x) <> (foldMap fm right)

  -- we can do manual versions of this later, technically foldMap+defaults are
  -- sufficient to define folds for now
  foldl f = foldlDefault f
  foldr f = foldrDefault f

-- traverse :: forall a b m. Applicative m => (a -> m b) -> Tree a -> m (Tree b)
instance Traversable Tree where
  traverse _ Leaf = pure Leaf
  traverse famb (Branch left val right) = ado
    l' <- traverse famb left
    v' <- famb val
    r' <- traverse famb right
  in (Branch l' v' r')

  -- sequence :: forall a m.   Applicative m => Tree (m a) -> m (Tree a)
  sequence famb = sequenceDefault famb
