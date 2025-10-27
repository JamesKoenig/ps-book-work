module Solutions.Group1 where

import Prelude

-- 1. (Easy) Write a recursive function `isEven` that returns `true` if and only
--           if its input is even
isEven :: Int -> Boolean
isEven n | n `mod` 2 == 0 = true
         | otherwise      = false

-- 2. (Medium) Write a recursive function `countEven` that counts the number of
--             even integers in an array.

