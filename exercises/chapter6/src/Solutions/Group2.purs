module Solutions.Group2 where

import Prelude

newtype Complex =
  Complex
  { real      :: Number
  , imaginary :: Number
  }

-- 1. (Easy) Define a `Show` instance for `Complex`.  Match the output format
--           expected by the tests (e.g. `1.2+3.4i`, `5.6-7.8i`, etc).


instance complexShow :: Show Complex where
  show (Complex {real, imaginary } ) =
    show real <> sign imaginary <> show imaginary <> "i"
    where sign :: Number -> String
          sign x | x < 0.0   = ""  -- show prepends a - on negatives
                 | otherwise = "+" -- but otherwise we need the +

-- 2. (Easy) Define an `Eq` instance for `Complex`

-- I should really do deriving since it's trivial
instance complexEq :: Eq Complex where
  eq (Complex z1) (Complex z2) = eq z1 z2

