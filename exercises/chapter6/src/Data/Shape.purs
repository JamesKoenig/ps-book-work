module Data.Shape where

-- in another show of oppositional defiance, I am not importing `Prelude`

import Data.Show         (class Show
                         ,show
                         )
import Data.Generic.Rep  (class Generic)
import Data.Eq           (class Eq
                         ,(==)
                         )
import Data.Ord          (class Ord
                         )
import Data.HeytingAlgebra ((&&))
import Data.Show.Generic (genericShow)
import Data.Semigroup    ((<>))
-- uncomment to use a -Generic version of eq, below
--import Data.Eq.Generic

-- Group 1, Exercise 1:
newtype Point = Point { x :: Number, y :: Number }

showPoint :: Point -> String
showPoint (Point { x, y }) =
  "(" <> show x <> ", " <> show y <> ")"

instance pointShow :: Show Point where
  show = showPoint

data Shape
  = Circle    Point Number
  | Rectangle Point Number Number
  | Line      Point Point
  | Text      Point String

-- Group 2, Exercise 5:
derive instance genericShape :: Generic Shape _
instance showShape :: Show Shape where
  -- point free would be ill advised if the recursive clipped type were here
  show = genericShow

-- Group 3, Exercise 7's required Eq implementation
instance eqPoint :: Eq Point where
  eq (Point {x: x1, y: y1}) (Point {x: x2, y: y2}) = x1 == x2
                                                  && y1 == y2

-- should I just use some generic? --in the future I definitely will.
instance eqShape :: Eq Shape where
  eq (Circle    p1    r1)
     (Circle    p2    r2) = p1 == p2
                         && r1 == r2
  eq (Rectangle p1 w1 h1)
     (Rectangle p2 w2 h2) = p1 == p2
                         && w1 == w2
                         && h1 == h2
  eq (Line      p1    q1)
     (Line      p2    q2) = p1 == p2
                         && q1 == q2

  eq (Text      p1    s1)
     (Text      p2    s2) = p1 == p2
                         && s1 == s2
  eq _ _ = false

-- generic version
--instance eqShape :: Eq Shape where
--  eq = genericEq
-- Look at how much EASIER that is!!!!

-- Group 3, Exercise 8's required Ord instance
--in the git log you can find a version of this where I did the ordering by hand
-- but uh, that was a lot.  In lieu of that:
derive instance ordPoint :: Ord Point
derive instance ordShape :: Ord Shape
-- as per the document we referenced for the chapter:
--https://github.com/purescript/documentation/blob/master/guides/Type-Class-Deriving.md
-- >For example, if you you'd like to be able to remove duplicates from an array
-- >of an ADT using nub, you need an Eq and Ord instance. Rather than writing
-- >these manually, let the compiler do the work.
