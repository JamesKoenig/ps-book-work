module Solutions.Group1 where

import Prelude (bind
               )

import Data.Maybe (Maybe
                  )
import Data.Array (head
                  ,tail
                  ,(!!)
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
