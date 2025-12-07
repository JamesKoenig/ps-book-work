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

-- ap :: forall m a b. Monad m => m (a -> b) -> m a -> m b
-- ap mf ma = do
--    f <- mf
--    a <- ma
--    pure (f a)
-- this is equivalent to:
--    ap mf ma = mf >>= (\f -> ma >>= (\a -> pure (f a)) )
-- for the Maybe monad:
--    ap :: forall m a b. Maybe (a -> b) -> Maybe a -> Maybe b
--    ap (Just f) (Just a) = (Just f) >>= (\f' -> (Just a)
--                                    >>= (\a'-> Just (f' a')))
-- Maybe's Bind instance is:
--    bind (Just x) k = k x
--    bind Nothing  _ = Nothing
-- Applying this to the above when neither is Nothing:
--    ap (Just f) (Just a) = (Just f) >>= (\f' -> (\a' -> Just (f' a')) a)
-- again on the outer bind:
--    ap (Just f) (Just a) = (\f -> (\a -> Just (a f) a) f)
-- reducing both lambdas, and reincluding the `Nothing` instance:
--    ap (Just f) (Just a) = Just (f a)
--    ap Nothing         _ = Nothing
-- apply's definition for Maybe:
--    apply :: forall (a :: Type) (b :: Type).
--      Maybe (a -> b) -> Maybe a -> Maybe b
--    apply (Just fn) x = fn <$> x
--    apply Nothing   _ = Nothing
-- functor definition for Maybe:
--    map fn (Just x)   = Just (fn x)
--    map _  _          = Nothing
-- substituting the first definition for both apply and map:
--    apply (Just f) (Just a) = f <$> (Just a) = Just (f a)
--  which means that apply (Just f) (Just a) = ap (Just f) (Just a)
-- in the situations where Nothing is encountered, for ap either argument being
-- nothing invokes the `bind Nothing _ = Nothing` result
--
-- for apply, the second case of the apply instance for maybe covers when the
-- Just (a -> b) is nothing, and the definition of map _ _ = Nothing will
-- pattern match when it's map fn Nothing, so the result is Nothing as well.

-- therefore in all cases they are the same.

-- 4. (Medium) Verify that the monad laws hold for the `Monad` instance for the
--             `Maybe` type, as defined in the `maybe` package.

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
