module Solutions.Group5 where

import Prelude
import Data.Picture (Shape(..))
import Data.Number (pi)

-- 1. (Medium) Extend the vector graphics library with a new operation `area`
--    that computes the area of a Shape.  For the purpose of this exercise, the
--    area of a line or piece is assumed to be zero.

-- HONESTLY I kinda feel like this stuff should just be class-instance functions
--   but that's WAY past the intention of this chapter

area :: Shape -> Number
area (Line      _ _ )   = 0.0
area (Text      _ _ )   = 0.0
area (Rectangle _ w h ) = w*h
area (Circle    _ r )   = r*pi*r
