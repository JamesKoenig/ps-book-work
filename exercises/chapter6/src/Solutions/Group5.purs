module Solutions.Group5 where

import Prelude

import Data.Hashable (class Hashable
                     ,hashEqual
                     ,hash
                     )
import Data.Array (nubByEq
                  )

-- 2. (Medium) Write a function `arrayHasDuplicates`, which tests if an array
--             has any duplicates based on both a hash and a value equality.
--             First, check for hash equality with the `hashEqual` function
--             then, check for equality with `==` if a duplicate pair of hashes
--             is found.

arrayHasDuplicates :: forall a. Hashable a => Array a -> Boolean
arrayHasDuplicates a = (a /= nubByEq go a)
  where go :: Hashable a => a -> a -> Boolean
        go x y = (hashEqual x y) && (eq x y)

-- 3. (Medium) Write a `Hashable` instance for the following newtype which
--             satisfies the type class law:
newtype Hour = Hour Int

instance Eq Hour where
  eq (Hour n) (Hour m) = n `mod` 12 == m `mod` 12
--  eq = eq `on` toHourInt
--             The newtype `Hour` and its `Eq` instance represent the type of
--             integers modulo 12, so that 1 and 13 are identified as equal,
--             for example.  Prove that the type class law holds for this
--             instance.

toHourInt :: Hour -> Int
toHourInt (Hour x) =  x `mod` 12

-- n.b. the above `eq` instance can just be:
--instance Eq Hour where
--  eq = eq `on` toHourInt

instance hashableHour :: Hashable Hour where
  hash = hash <<< toHourInt

-- #the law is that a == b implies (hash a) == (hash b)#
-- Assume law holds for Hashable Int.
-- Via hashableHour:
--    (hash :: Hour -> HashCode) = (hash :: Int -> HashCode) <<< toHourInt
-- Via applying composition e.g. h = (f <<< g) => h x = f (g x)
--     hash (Hour x) = (hash :: Int -> HashCode) (toHourInt (Hour x))
-- Via definition of toHourInt:
--     hash (Hour x) = hash (x `mod` 12)
-- Let x,y be Ints and let m = x `mod` 12, n = y `mod` 12
--(N.B. n,m are also Ints)
-- Applying m:
--     hash (Hour x) = hash m
--(N.B. left is hash :: Hour -> HashCode, right is hash :: Int -> HashCode)
-- using the above, and the equivalent for n:
--     (hash (Hour x)) == (hash (Hour y)) = hash m == hash n      -- Lemma 1
-- On the other hand
--     (Hour x) == (Hour y) = x `mod` 12 == y `mod` 12 --via Eq Hour
-- Applying m,n's definitions:
--     (Hour x) == (Hour y) = m == n                              -- Lemma 2
-- Note that as m,n are both ints so the law a == b => (hash a) == (hash b)
-- has already been assumed to start with.  This means as:
--     (Hour x) == (Hour y) => hash (Hour x) == hash (Hour y)
-- can be shown with m,n via Lemma 1 & 2 above as:
--     m == n => (hash m) == (hash n)
-- which is equivalent to the Hash Int instance of the law for m,n
-- (done)


-- 4. (Difficult) Prove the type class laws for the `Hashable` instances for
--                `Maybe`, `Either`, and `Tuple`

-- pt1. Maybe
-- The law is that a == b => (hash a) == (hash b) ( => being implies )
-- Assume the law holds for (Hashable a)
-- Via Nothing pattern of hash:
--    hash Nothing = hashCode 0
-- This means Nothing == Nothing => 0 == 0 which is true trivially
-- Via (Just a) pattern of hash:
--    hash (Just a) = hashCode 1 `combineHashes` hash a
-- Let `h_a` be the result of `hash a`
-- Applying definition of combineHashes:
--    hashcode 1 `combineHashes` h_a = hashCode (73*1 + 51*h_a)
-- applying hashCode:
--    hashCode (73+51*h_a) = (73 + 51*h_a) `mod` 65535
-- let x,y \in A where A is an instance of Hashable, & let hx,hy be their hash
-- codes.  Assume the law holds for A such that x == y  => hx == hy
-- and let a == b, then:
--    (hash (Just a)) == (hash (Just b)) =
--        (73 + 51*hx) `mod` 65535  == (73 + 51*hy) `mod` 65535
