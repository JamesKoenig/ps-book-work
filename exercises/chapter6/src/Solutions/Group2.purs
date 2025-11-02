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

-- 3. (Medium) Define a `Semiring` instance for `Complex`.

-- TODO: look at what `Data.Newtype` `wrap` and `over2` can do for this
--       it's neat but I really wish the book went over stuff like it in its
--       own chapter section.

-- N.B. you can just add the records together for add
instance complexSemiring :: Semiring Complex where
  add (Complex {real: a, imaginary: b} )
      (Complex {real: c, imaginary: d} ) = Complex { real:      (a+c)
                                                   , imaginary: (b+d)
                                                   }

  mul (Complex {real: a, imaginary: b} )
      (Complex {real: c, imaginary: d} ) = Complex { real:      (a*c)-(b*d)
                                                   , imaginary: (a*d)+(c*b)
                                                   }

  zero = (Complex { real: 0.0, imaginary: 0.0 } )

  one  = (Complex { real: 1.0, imaginary: 0.0 } )

-- 4. (Easy) Define a `Ring` instance for `Complex`

-- apparently you can just do `derive newtype instance Ring Complex`
-- or, alternatively, you can just subtract the records from one another
instance complexRing :: Ring Complex where
  sub (Complex {real: a, imaginary: b} )
      (Complex {real: c, imaginary: d} ) = Complex { real:      (a-c)
                                                   , imaginary: (b-d)
                                                   }


-- 5. (Medium) Derive (via `Generic`) a `Show` instance for `Shape`.  How does
--             the amount of code written and `String` output compare to
--             `showShape` from the previous chapter?
-- See Data.Shape (in this chapter) for the solution to this, code-wise.
-- Seeing as the output is kinda like:
-- ```haskell
--  > show (Circle (Point {x: 0.0, y: 0.0 }) 1.0 )
--  "(Circle (0.0, 0.0) 1.0)"
-- ```
-- it's kinda way less readable than Chapter 4's:
-- `"Circle [center: (0.0, 0.0), radius: 1.0]"`
-- neither really fits the goal in this chapter of somethign that can be pasted
-- back into PSCi or a `.purs` file.
