module Diagonal where

-- these are also in `Prelude`
import Data.Semiring ((*)
                     ,(+)
                     )
import Data.Number (sqrt)

diagonal :: Number -> Number -> Number
diagonal x y = sqrt z2
  where z2 = x*x + y*y
