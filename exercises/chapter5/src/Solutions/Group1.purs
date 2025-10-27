module Solutions.Group1 where

import Prelude

isEven :: Int -> Boolean
isEven n | n `mod` 2 == 0 = true
         | otherwise      = false
