module Solutions.Group5 where

import Prelude

import Data.Hashable (class Hashable
                     ,hashEqual
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
