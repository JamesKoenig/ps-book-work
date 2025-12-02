module Solutions.Group1 where

import Prelude

import Data.Maybe
import Data.Array (head
                  ,tail
                  )

-- 1. (Easy) Write a function `third` that returns the third element of an array
--           with three or more elements.  Your function should return an
--           appropriate `Maybe` type.

third :: forall (a :: Type). Array a -> Maybe a
third as = do
  firstTail  <- tail as
  secondTail <- tail firstTail
  head secondTail 
