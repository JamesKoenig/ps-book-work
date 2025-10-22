module Test.MySolutions where

import Prelude

--based upon statements in the assignment we're going to assume we're not facing
-- negatives.  It'd be nice 
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)


