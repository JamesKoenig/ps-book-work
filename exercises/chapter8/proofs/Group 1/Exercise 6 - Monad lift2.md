# Exercise 6:
>(Difficult) Every monad has a default functor instance given by:
```haskell
map f a = do
    x <- a
    pure (f x)
```
>Use the monad laws to prove that for any monad, the following holds:
```haskell
lift2 f (pure a) (pure b) = pure (f a b)
```
>Where the `Apply` instance uses the `ap` function
```haskell
ap :: forall m a b. Monad m => m (a -> b) -> m a -> m b
ap mf ma = do
  f <- mf
  a <- ma
  pure (f a)
```
>Recall that  `lift2` was defined as follows
```haskell
lift2 :: forall f a b c. Apply f => (a -> b -> c) -> f a -> f b -> f c 
lift2 f a b = f <$> a <*> b
```

## Proof, using the above:
```haskell
lift2 f (pure a) (pure b)
-- substitute lift2 def'n
f <$> (pure a) <*> (pure b)
-- evaluating the fmap on the left:
f <$> (pure a) = do
    x <- (pure a)
    pure (f x)
-- this reduces to
pure (f a)
-- now to the apply
ap (pure (f a)) (pure b) = do
    f' <- (pure (f a))
    x' <- (pure b)
    pure (f' x')
-- simplyfing
pure (f a b)
-- QED
```
