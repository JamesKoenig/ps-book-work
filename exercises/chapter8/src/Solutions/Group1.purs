module Solutions.Group1 where

import Prelude (bind
               ,(<<<)
               ,($)
               ,(+)
               ,pure
               ,(>>=)
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
import Control.Monad (class Monad)
import Data.List (List(Nil)
                 ,(:)
                 )

-- 1. (Easy) Write a function `third` that returns the third element of an array
--           with three or more elements.  Your function should return an
--           appropriate `Maybe` type.

thirdM :: forall (a :: Type). Array a -> Maybe a
thirdM xs = do
  firstTail  <- tail xs
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

-- 3. (Medium) Confirm that the `ap` function and the `apply` operator agree for
--             the `Maybe` monad.

-- See `proofs/Group 1/Exercise 3 - Maybe ap and apply agree.md`
--   for the solution.

-- 4. (Medium) Verify that the monad laws hold for the `Monad` instance for the
--             `Maybe` type, as defined in the `maybe` package.

-- See `proofs/Group 1/Exercise 4 - Maybe adherence to Monad Laws.md`
--   for the solution.

-- 5. (Medium) Write a function `filterM` which generalizes the `filter`
--             function on lists.  Your function should have the following type
--             signature:
filterM :: forall m a. Monad m => (a -> m Boolean) -> List a -> m (List a)
filterM _ Nil = pure Nil
filterM tm (x:xs) = do
  keep  <- tm x
  recur <- filterM tm xs
  if keep
    then pure $ x : recur
    else pure recur

filterMBound :: forall m a. Monad m => (a -> m Boolean) -> List a -> m (List a)
filterMBound  _ Nil = pure Nil
filterMBound tm (x:xs) =
  tm x >>=
    \keep ->
      filterMBound tm xs >>=
        \recur ->
          pure (if keep
                then x : recur
                else recur)
