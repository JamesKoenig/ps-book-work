module Solutions.Group1 where

import Prelude (bind
               ,(<<<)
               ,($)
               ,(+)
               )

import Data.Maybe (Maybe
                  )
import Data.Array (head
                  ,tail
                  ,(!!)
                  ,sort
                  ,nub
                  ,foldM
                  )

-- 1. (Easy) Write a function `third` that returns the third element of an array
--           with three or more elements.  Your function should return an
--           appropriate `Maybe` type.

thirdM :: forall (a :: Type). Array a -> Maybe a
thirdM as = do
  firstTail  <- tail as
  secondTail <- tail firstTail
  head secondTail 

third' :: forall (a :: Type). Array a -> Maybe a
third' = (_!!2)

third :: forall ( a :: Type). Array a -> Maybe a
third = thirdM --replace thirdM with third' to use (!!) ver

-- 2. (Medium) Write a function `possibleSums` which uses foldM to determine
--             all possible totals that could be made using a set of coins.
--             The coins will be specified as an array which contains the value
--             of each coin.

possibleSums :: Array Int -> Array Int
possibleSums xs = nub <<< sort $ foldM (\x y -> [x,y,x+y]) 0 xs
