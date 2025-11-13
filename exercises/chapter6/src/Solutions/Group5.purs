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
