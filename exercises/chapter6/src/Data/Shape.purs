module Data.Shape where

import Data.Show         (class Show
                         ,show
                         )
import Data.Generic.Rep  (class Generic)
import Data.Show.Generic (genericShow)
import Data.Semigroup    ((<>))

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
