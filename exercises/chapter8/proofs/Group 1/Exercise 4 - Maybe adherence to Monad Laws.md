# Exercise 4:
> _Verify that the monad laws hold for the `Monad` instance for the `Maybe` type, as defined in the `maybe` package._

The laws that need to be proven are:
1. Left Identity
2. Right Identity
3. Associativity

## Right identity:
The right identity requires us to show:
```haskell
do
  x <- expr
  pure x
```
to be the same as:
```haskell
expr
```

The first expression is the same as:
```haskell
   (bind              :: Maybe a -> (a -> Maybe b) -> Just b )
      (expr           :: Maybe a                             )
      ((\x -> pure x) ::       a -> Maybe a                  )
```
Nota bene: expr is either `Nothing` or `Just a`.

the definition of `bind` from `Monad Maybe` is:
```haskell
bind Nothing  _ = Nothing
bind (Just x) k = k x
```
given `pure` is defined as `Just` for the `Applicative Maybe` instance, `bind`
becomes either:
```haskell
bind (Just x) Just = Just x
```
Which is the same as `expr` when `expr` is `Just a`, or:
```haskell
bind Nothing  Just = Nothing
```
Which is the same as `expr` when `expr` is `Nothing`.

Therefore, by exhaustion, the right identity law holds for Maybe
## Left Identity:
The left identity is that:
```haskell
do
  x <- pure y
  next
```
is the same as:
```haskell
next
```
> "after the name `x` has been replaced with the expression `y`"

Because that doesn't make a lot of sense I'm going to use
[the version from the Control.Monad documentation][control-monad] which states:
```haskell
-- left identity
pure x >>= f = f x
```
substituting in `pure x` for its `Just` from `Applicative Maybe`, using `bind`
and adding type annotations for `Maybe`:
```haskell
(bind :: Maybe a -> (a -> Maybe b)  -> Maybe b)
  (Just x :: Maybe a)
  (f      :: a -> Maybe b)
```
substituting `bind (Just x) k = k x` from the definition of a non-`Nothing`
Maybe this becomes:
```haskell
f x
```
Q.E.D.

## Associativity:
the associative law states that:
```haskell
do
  y <- do
    x <- m1
    m2
  m3
```
and
```haskell
do
  x <- m1
  y <- m2
  m3
```
are equivalent, as are any variations.

the inner do of the first one becomes
```haskell
do
  y <- (m1 >>= \x -> m2)
  m3
```
which becomes
```haskell
m1 >>= \x -> m2 >>= \y -> m3
```
one can show that the second form becomes the same thing:
```haskell
do
  x <- m1
  y <- m2
  m3
-- becomes
m1 >>= \x -> m2 >>= \y -> m3
```

[control-monad]: https://pursuit.purescript.org/packages/purescript-prelude/6.0.0/docs/Control.Monad#t:Monad
