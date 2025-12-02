module Solutions.Group3 where

import Prelude
import Data.Generic.Rep  (class Generic)
import Data.Show.Generic (genericShow)
import Data.Foldable     (class Foldable
                         ,foldMap
                         ,foldl
                         ,foldr
                         )
import Data.Traversable (class Traversable
                        ,traverse
                        ,sequence
                        )
import Data.Maybe       (Maybe(..))
import Data.AddressBook (Address
                        ,PhoneNumber
                        ,phoneNumber
                        ,address
                        ,PhoneType(WorkPhone,CellPhone)
                        )
import Data.Validation.Semigroup (V)
import Data.AddressBook.Validation (nonEmpty
                                   ,Errors
                                   ,validateAddress
                                   ,validatePhoneNumbers
                                   )

-- 1. (Easy) Write an `Eq` and `Show` instance for the following binary tree
--           data structure
data Tree a = Leaf | Branch (Tree a) a (Tree a)

derive instance eqTree :: Eq a => Eq (Tree a)
derive instance genericTree :: Generic (Tree a) _
-- this can't be a point free version for some reason
instance showTree :: Show a => Show (Tree a) where
  show tree = genericShow tree

-- 2. (Medium) Write a `Traversable` instance for `Tree a`, which combines
--             side-effects left-to-right

-- Traverasable requires Functor and Foldable constraints
instance Functor Tree where
  map _ Leaf = Leaf
  map f (Branch left val right) = Branch (f <$> left) (f val) (f <$> right)

-- we did this before in Ch6 Grp3 Ex5 & Ex6-- that was hard
-- the instance fns for foldable are:
--  foldr   :: forall a b. (a -> b -> b) -> b -> f a -> b
--  foldl   :: forall a b. (b -> a -> b) -> b -> f a -> b
--  foldMap :: forall a m. Monoid m => (a -> m) -> f a -> m
instance Foldable Tree where
-- get the easiest one oout of the way
  foldMap _  Leaf = mempty
  foldMap fm (Branch left x right) =
    (foldMap fm left) <> (fm x) <> (foldMap fm right)
-- the above is technically sufficient along with foldlDefault/foldrDefault

  -- we can do manual versions of this later, technically foldMap+defaults are
  -- sufficient to define folds for now
  foldl :: forall a b. (b -> a -> b) -> b -> Tree a -> b
  foldl _ acc Leaf = acc
  foldl fbab acc (Branch left val right) =
    foldl fbab leftFolded right
    where leftFolded = flip fbab val $ foldl fbab acc left
  --foldl f a (Branch l v r) = foldl f (f (foldl f a l) v) r

  foldr :: forall a b. (a -> b -> b) -> b -> Tree a -> b
  foldr _    acc Leaf = acc
  foldr fabb acc (Branch left val right) =
    foldr fabb rightFolded left
    where rightFolded = fabb val $ foldr fabb acc right
  --foldr f a (Branch l v r) = foldr f (f v (foldr f a r)) l

-- traverse :: forall a b m. Applicative m => (a -> m b) -> Tree a -> m (Tree b)
instance Traversable Tree where
  traverse _ Leaf = pure Leaf
  traverse famb (Branch left val right) = ado
    l' <- traverse famb left
    v' <- famb val
    r' <- traverse famb right
  in (Branch l' v' r')
  --traverse f (Branch l v r) = Branch <$> traverse f l
  --                                   <*> f v
  --                                   <*> traverse f r

  -- sequence :: forall a m.   Applicative m => Tree (m a) -> m (Tree a)
  sequence Leaf = pure Leaf
  sequence (Branch left mval right) = Branch <$> sequence left
                                             <*> mval --recall this is `m a`
                                             <*> sequence right
  -- add sequenceDefault to the Data.Traversable imports to use this
  --sequence famb = sequenceDefault famb

-- 3. (Medium) Write a function:
-- ```
traversePreOrder :: forall a m b. Applicative m => (a -> m b)
                                                -> Tree a
                                                -> m (Tree b)
-- ```
--             that performs a pre-order traversal of the tree
traversePreOrder _ Leaf = pure Leaf
traversePreOrder famb (Branch left val right) = ado
  v' <- famb val
  l' <- traversePreOrder famb left
  r' <- traversePreOrder famb right
  in (Branch l' v' r')

-- 4. (Medium) Write a function `traversePostOrder` that peforms post-order
--             traversal of the tree where effects are executed left-right-foot
traversePostOrder :: forall a m b. Applicative m => (a -> m b)
                                                 -> Tree a
                                                 -> m (Tree b)
traversePostOrder _ Leaf = pure Leaf
traversePostOrder famb (Branch left val right) = ado
  l' <- traversePostOrder famb left
  r' <- traversePostOrder famb right
  v' <- famb val
  in (Branch l' v' r')

-- alternatively:
--traversePostOrder f (Branch l v r) =
--  (\l r v -> Branch l v r) <$> traversePostOrder f l
--                           <*> traversePostOrder f r
--                           <*> f v

-- 5. (Medium) Create a new version of the `Person` type where the `homeAddress`
--             field is optional (using `Maybe`) then write a new version of
--             `validatePerson` (renamed as `validatePersonOptionalAddress`) to
--             validate this new `Person`.

type AlternatePerson = { firstName   :: String
                       , lastName    :: String
                       , homeAddress :: Maybe Address
                       , phones      :: Array PhoneNumber
                       }

alternatePerson :: String -> String -> Maybe Address
                   -> Array PhoneNumber -> AlternatePerson
alternatePerson firstName lastName homeAddress phones = { firstName
                                                        , lastName
                                                        , homeAddress
                                                        , phones
                                                        }

sampleAltPerson :: AlternatePerson
sampleAltPerson = alternatePerson "John" "Smith"
                  (Just $ address "123 Sample Street" "San Jose" "CA" )
                  [(phoneNumber CellPhone "555-555-5555")
                  ,(phoneNumber WorkPhone "555-555-5000")
                  ]

validatePersonOptionalAddress :: AlternatePerson -> V Errors AlternatePerson
validatePersonOptionalAddress ap =
  alternatePerson <$> nonEmpty "First Name"                ap.firstName
                  <*> nonEmpty "Last Name"                 ap.lastName
                  <*> traverse validateAddress             ap.homeAddress
                  <*> validatePhoneNumbers "Phone Numbers" ap.phones

-- Comment the above definition and uncomment the below to use the `ado` ver
--validatePersonOptionalAddress = validatePersonOptionalAddressAdo
validatePersonOptionalAddressAdo :: AlternatePerson -> V Errors AlternatePerson
validatePersonOptionalAddressAdo ap = ado
  fName <- nonEmpty "First Name"                ap.firstName
  lName <- nonEmpty "Last Name"                 ap.lastName
  mAddy <- traverse validateAddress             ap.homeAddress
  pNums <- validatePhoneNumbers "Phone Numbers" ap.phones
  in (alternatePerson fName lName mAddy pNums)

-- 6. (Difficult) Write a function `sequenceUsingTraverse` which behaves like
--                `sequence`, but is written in terms of `traverse`.

-- I actaully did this earlier before defining `sequence` for `Traversable Tree`
sequenceUsingTraverse :: forall (@t :: Type -> Type)
                                ( a :: Type)
                                ( m :: Type -> Type).
                                  Traversable t => Applicative m =>
                                                  t (m a) -> m (t a)
sequenceUsingTraverse t = traverse (\x -> x) t -- alternatively define id


-- 7 (Difficult) Write a function `traverseUsingSequence` which behaves like
--               `traverse`, but is written in terms of `sequence`
traverseUsingSequence :: forall (@t :: Type -> Type)
                                ( a :: Type )
                                ( b :: Type )
                                ( m :: Type -> Type).
                                  Traversable t => Applicative m =>
                                                  (a -> m b) -> t a -> m (t b)
traverseUsingSequence f t = sequence $ f <$> t

