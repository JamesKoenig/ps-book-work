module Solutions.Group3 where

import Prelude

import Data.Picture (Shape(Circle
                          ,Text
                          ,Rectangle
                          ,Line
                          )
                    ,origin
                    )

-- I'm good using the range shorthand here for `Just` and `Nothing`
import Data.Maybe (Maybe(..))

-- ### Group 3: ###

-- 1. (Easy) Write a function `circleAtOrigin` which constructs a `Circle` (of
--    type `Shape`) centered at the origin with a radius `10.0`.

circleAtOrigin :: Shape
circleAtOrigin = Circle origin 10.0

-- 2. (Medium) Write a function `doubleScaleAndCenter` that scales the size of a
--    `Shape` by a factor of 2.0 and centers it at the origin

doubleScaleAndCenter :: Shape -> Shape
--in order of ease of implementation
doubleScaleAndCenter (Text      _ text  ) = Text      origin  text
doubleScaleAndCenter (Circle    _ radius) = Circle    origin (radius*2.0)
doubleScaleAndCenter (Rectangle _ w h   ) = Rectangle origin (w*2.0) (h*2.0)
-- and now the one that ruins the formatting pattern:
doubleScaleAndCenter (Line start end ) = let width  = end.x - start.x
                                             height = end.y - start.y
                                          in
                                            Line { x: -width, y: -height }
                                                 { x:  width, y:  height }

-- 3. (Medium) Write a function `shapeText` which extracts text from a `Shape`
--    it should return `Maybe String`
shapeText :: Shape -> Maybe String
shapeText (Text _ text) = Just text
shapeText _             = Nothing
