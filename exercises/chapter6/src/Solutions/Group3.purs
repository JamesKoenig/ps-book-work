module Solutions.Group3 where

import Prelude

import Data.Generic.Rep  (class Generic)
import Data.Foldable     (class Foldable
                         ,foldr
                         ,foldl
                         ,foldMap
                         )
import Data.Show.Generic (genericShow)
import Data.Shape (Shape)
import Data.Array (nubEq
                  ,nub
                  )

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

-- 4. (Medium) Given any type `a` with an instance of `Ord`, we can add a new
--             "infinite" value that is greater than any other value:
data Extended a = Infinite | Finite a
--             Write an `Ord` instance for `Extended a` that reuses the `Ord`
--             instance for `a`

-- because `class Eq a <= Ord a` we have to define eq for `Extended` in order
--  for it to have an Ord instance
instance extendedEq :: Eq a => Eq (Extended a) where
  eq Infinite   Infinite   = true
  eq (Finite x) (Finite y) = eq x y
  eq _          _          = false

instance extndedOrd :: Ord a => Ord (Extended a) where
  compare Infinite   Infinite   = EQ
  compare Infinite   _          = GT
  compare _          Infinite   = LT
  compare (Finite x) (Finite y) = compare x y

-- 5. (Difficult) write a `Foldable` instance for `NonEmpty`.

-- `Foldable` is `(Type -> Type) -> Constraint` so it takes `NonEmpty`
instance nonEmptyFoldable :: Foldable NonEmpty where
  -- here f2 denotes a binary function (a -> b -> b)
  foldr   f2 default (NonEmpty x xs) = x `f2` foldr f2 default xs
  -- here f2 denotes a binary function (b -> a -> b)
  foldl   f2 default (NonEmpty x xs) = foldl f2 (f2 default x) xs
  -- here fm denotes a unary function (a -> m)
  foldMap fm         (NonEmpty x xs) = append (fm x) $ foldMap fm xs
  -- alternatively:
--foldMap fm (NonEmpty x xs) = fm x <> foldMap fm xs

-- 6. (Difficult) Given a type constructor `f` which defines an ordered
--                container (and so has a `Foldable` instance), we can create a
--                new container type that includes an extra element at the
--                front:
data OneMore f a = OneMore a (f a)
--                The container `OneMore f` also has an ordering, where the new
--                element comes before any element of `f`.  Write a `Foldable`
--                intsance for `OneMore f`.

-- I'm not sure if I missed something but this seems to be exactly like 5
--  with abstraction...  Maybe that's the point that's supposed to click
instance oneMoreFoldable :: Foldable f => Foldable (OneMore f) where
  foldr f2 default (OneMore x fx) = x `f2` foldr f2 default fx
  foldl f2 default (OneMore x fx) = foldl f2 (default `f2` x) fx
  foldMap fm (OneMore x fx) = fm x <> foldMap fm fx

-- 7. (Medium) Write a `dedupShapes :: Array Shape -> Array Shape` function that
--             removes duplicate `Shape`s from an array using the nubEq function
-- See `Data.Shape` (in this chapter) for the Eq instance
dedupShapes :: Array Shape -> Array Shape
dedupShapes = nubEq

-- 8. (Medium) Write a `dedupShapesFast` function which is the same as
--             `dedupShapes`, but uses the more efficient `nub` function

dedupShapesFast :: Array Shape -> Array Shape
dedupShapesFast = nub

