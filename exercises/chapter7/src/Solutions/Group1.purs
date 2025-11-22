module Solutions.Group1 where

import Prelude
import Data.Maybe (Maybe(..))
import Control.Apply (lift2)

-- 1. (Medium) Write the versions of the numeric operators `+`, `-`, '*', and
--             `/` which work with optional arguments (i.e. arguments wrapped in
--             `Maybe`) and return a value wrapped in `Maybe`.  Name these
--             functions `addMaybe`, `subMaybe`, `mulMaybe`, and `divMaybe`

-- recall (+) :: forall (@a :: Type). Semiring a => a -> a -> a
addMaybe :: forall a. Semiring a => Maybe a -> Maybe a -> Maybe a
addMaybe = lift2 (+) --point-free

-- (-) aka sub is defined for Ring instances
subMaybe :: forall a. Ring a => Maybe a -> Maybe a -> Maybe a
subMaybe fx fy = ado
  x <- fx
  y <- fy
  in (x - y)

-- write this one out long-form without apply
mulMaybe :: forall (@a :: Type). Semiring a => Maybe a -> Maybe a -> Maybe a
mulMaybe _ Nothing = Nothing
mulMaybe Nothing _ = Nothing
mulMaybe (Just x) (Just y) = Just (x * y)

-- using <$> <*> chain
divMaybe :: forall (@a :: Type). EuclideanRing a =>
                  Maybe a -> Maybe a -> Maybe a
divMaybe x y = (/) <$> x <*> y

-- 2. (Medium) Extend the above exercise to work with all `Apply` types (not
--             just `Maybe`).  Name these new functions `addApply`, `subApply`,
--             `mulApply`, and `divApply`.

--Other than signature, all of these are the same except for
--  mulMaybe <-> mulApply

addApply :: forall f a. Apply f => Semiring a => f a -> f a -> f a
addApply = lift2 (+)

subApply :: forall f a. Apply f => Ring a => f a -> f a -> f a
subApply fx fy = ado
  x <- fx
  y <- fy
  in (x - y)

-- because we can't use the previous ones, we're going to use `map` & `apply`
mulApply :: forall (@f :: Type -> Type) (@a :: Type). Apply f => Semiring a =>
              f a -> f a -> f a
mulApply x y = apply (map (*) x) y

-- again, using <$> <*> chained together
divApply :: forall (@f :: Type -> Type) (@a :: Type).
            Apply f => EuclideanRing a =>
            f a -> f a -> f a
divApply x y = (/) <$> x <*> y
