module Solutions.Group1 where

import Data.Array (head
                  ,tail
                  )
import Data.Maybe (Maybe
                  ,fromMaybe
                  )
import Data.Enum (fromEnum)
import Data.Foldable (foldl)
import Prelude

-- 1. (Easy) Write a recursive function `isEven` that returns `true` if and only
--           if its input is even
isEven :: Int -> Boolean
isEven n | n `mod` 2 == 0 = true
         | otherwise      = false

-- 2. (Medium) Write a recursive function `countEven` that counts the number of
--             even integers in an array.

-- they want me to do this purely recursively so I'm not going to use `fmap` or
--   any foldls for the default defn
countEven :: Array Int -> Int
countEven [] = 0
countEven arr = (headCount $ head arr) + (countEven $ fromMaybe [] $ tail arr)
  where headCount :: Maybe Int -> Int
        headCount = fromBool <<< isEven <<< (fromMaybe 0)
        fromBool  :: Boolean -> Int
        fromBool  b | b == true = 1
                    | otherwise = 0

-- using cheats as it were-- I'm using `fromEnum` though b/c I didn't like
-- writing out a helper function
countEven' :: Array Int -> Int
countEven' arr = foldl (+) 0 $ fromEnum <<< isEven <$> arr
