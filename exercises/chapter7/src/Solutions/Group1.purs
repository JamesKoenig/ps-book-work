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
