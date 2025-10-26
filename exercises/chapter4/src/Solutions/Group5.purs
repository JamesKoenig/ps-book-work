module Solutions.Group5 where

import Prelude
import Data.Picture (Shape(..))
import Data.Number (pi
                   ,nan
                   )

-- 1. (Medium) Extend the vector graphics library with a new operation `area`
--    that computes the area of a Shape.  For the purpose of this exercise, the
--    area of a line or piece is assumed to be zero.

-- HONESTLY I kinda feel like this stuff should just be class-instance functions
--   but that's WAY past the intention of this chapter

area :: Shape -> Number
area (Line      _ _ )    = 0.0
area (Text      _ _ )    = 0.0
area (Rectangle _ w h )  = w*h
area (Circle    _ r )    = r*pi*r

-- Hello from the future! This is to allow everything to compile after Group 5
-- Exercise 2's addition of `Clipped` objects to the `Shape` data type.

--I'm not implementing area for a clipped picture right now b/c the exercise is
--  only asking and testing for shapeBounds to work.
--Calculating the area of the circle segments outside of a bound (clip)
--  would be a really nice calculus exercise though.  Possibly a good whiteboard
--  question or something.
area (Clipped   _ _ _ _) = nan

-- 2. (Difficult) Extend the `Shape` type with a new data constructor `Clipped`,
--                which clips another `Picture` to a rectangle.  Extend the
--                `shapeBounds` function to compute the bounds of a clipped
--                picture.  Note that this makes `Shape` into a recursive data
--                type.
--                _Hint_: The compiler will walk you through extending the other
--                other functions as required.

-- the work for this kinda necessarily goes over to the Data.Picture module
