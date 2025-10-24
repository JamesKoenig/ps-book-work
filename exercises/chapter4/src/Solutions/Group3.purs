module Solutions.Group3 where

-- import Prelude

import Data.Picture (Shape(Circle
                          )
                    ,origin
                    )

-- ### Group 3: ###

-- 1. (Easy) Write a function `circleAtOrigin` which constructs a `Circle` (of
--    type `Shape`) centered at the origin with a radius `10.0`.

circleAtOrigin :: Shape
circleAtOrigin = Circle origin 10.0
