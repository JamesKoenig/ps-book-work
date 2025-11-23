module Solutions.Group3 where

import Prelude
import Data.Generic.Rep  (class Generic)
import Data.Show.Generic (genericShow)
-- import Data.Eq.Generic   (genericEq)

-- 1. (Easy) Write an `Eq` and `Show` instance for the following binary tree
--           data structure
data Tree a = Leaf | Branch (Tree a) a (Tree a)

derive instance eqTree :: Eq a => Eq (Tree a)
derive instance genericTree :: Generic (Tree a) _
-- this can't be a point free version for some reason
instance showTree :: Show a => Show (Tree a) where
  show tree = genericShow tree

