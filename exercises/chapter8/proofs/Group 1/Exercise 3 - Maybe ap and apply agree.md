# Exercise 3:
> _Confirm that the `ap` function and the `apply` operator agree for the `Maybe` monad._

## We begin with the definition of `ap` on `Maybe`
```haskell
ap :: forall m a b. Monad m => m (a -> b) -> m a -> m b
ap mf ma = do
   f <- mf
   a <- ma
   pure (f a)
```
this is equivalent to:
```haskell
ap mf ma = mf >>= (\f -> ma >>= (\a -> pure (f a)) )
```
for the Maybe monad:
```haskell
ap :: forall m a b. Maybe (a -> b) -> Maybe a -> Maybe b
ap (Just f) (Just a) = (Just f) >>= (\f' -> (Just a)
                                >>= (\a' -> Just (f' a')))
```
Maybe's Bind instance is:
```haskell
bind (Just x) k = k x
bind Nothing  _ = Nothing
```
## We then consider the case where neither argument is `Nothing`
```haskell
ap (Just f) (Just a) = (Just f) >>= (\f' -> (\a' -> Just (f' a')) a)
```
again on the outer bind:
```haskell
ap (Just f) (Just a) = (\f -> (\a -> Just (a f) a) f)
```
reducing both lambdas
```haskell
ap (Just f) (Just a) = Just (f a)
```
apply's definition for Maybe:
```haskell
apply :: forall (a :: Type) (b :: Type). Maybe (a -> b) -> Maybe a -> Maybe b
apply (Just fn) x = fn <$> x
apply Nothing   _ = Nothing
```
functor definition for Maybe:
```haskell
map fn (Just x)   = Just (fn x)
map _  _          = Nothing
```
substituting the first definition for both apply and map:
```haskell
apply (Just f) (Just a) = f <$> (Just a)
                        = Just (f a)
```
which means that `apply (Just f) (Just a) = ap (Just f) (Just a)`

## On the other hand, in the situations where `Nothing` is encountered:

### For `ap`:
either argument being `Nothing` invokes the
```haskell
bind Nothing _ = Nothing
```
pattern of the `Bind Maybe` instance, so the result is `Nothing`

### For `apply`:
the second case of the `apply` instance for `Maybe` covers when the
first (`Just (a -> b)`) argument is nothing, i.e.
```haskell
apply Nothing _ = Nothing
```
and the catch-all definition of `map`:
```haskell
map _ _ = Nothing
```
will pattern match when `map fn Nothing` is called,
so the result is `Nothing` if either argument is `Nothing` as well.

## Therefore
in all cases they are the same.
