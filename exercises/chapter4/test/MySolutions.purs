module Test.MySolutions where

import Prelude
import Data.Person (Person
                   )

-- ### Group 2: ###

-- 1. (Easy) write a function `sameCity` which uses record patterns to test
--    whether two `Person` records live in the same city.

sameCity :: Person -> Person -> Boolean
sameCity { address: { city: citL } } { address: { city: citR }} = citL == citR


-- 2. (Medium) write a function `fromSingleton`
fromSingleton :: forall a. a -> Array a -> a
fromSingleton default [x] = x
fromSingleton default _   = default

