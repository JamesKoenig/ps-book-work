module Data.Shape where

import Data.Show         (class Show)
import Data.Generic.Rep  (class Generic)
import Data.Show.Generic (genericShow)

-- this is marginally uncomfortable since the normal flow _feels_ like the
--    `Solutions` code should depend on the library code like `Data` etc
--    But in this case everything about `Point` is technically already contained
--    and finished with regards to the assignments, and the the added instance
--    behavior they're a part of

import Solutions.Group1 (Point)

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
