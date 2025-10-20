module Solutions.Circle where
import Data.Semiring ((*))
import Data.Number (pi) as Number

circleArea :: Number -> Number
circleArea r = r*r*Number.pi
