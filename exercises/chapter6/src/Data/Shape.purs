module Data.Shape where

import Data.Show         (class Show
                         ,show
                         )
import Data.Generic.Rep  (class Generic)
import Data.Eq           (class Eq
                         ,(==)
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
