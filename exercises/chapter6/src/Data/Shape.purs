module Data.Shape where

-- in another show of oppositional defiance, I am not importing `Prelude`

import Data.Show         (class Show
                         ,show
                         )
import Data.Generic.Rep  (class Generic)
import Data.Eq           (class Eq
                         ,(==)
                         )
import Data.Ord          (class Ord
                         ,compare
                         ,Ordering(..)
                         )
import Data.Function     (($))
import Data.Semiring     ((*)
                         ,(+)
                         )
import Data.Ring         ((-))
import Data.Number       (sqrt)
import Data.HeytingAlgebra ((&&))
import Data.Show.Generic (genericShow)
import Data.Semigroup    ((<>))
-- uncomment to use a -Generic version of eq, below
--import Data.Eq.Generic

-- Group 1, Exercise 1:
newtype Point = Point { x :: Number, y :: Number }
mkPt x y = Point {x, y}

showPoint :: Point -> String
showPoint (Point { x, y }) =
  "(" <> show x <> ", " <> show y <> ")"

instance pointShow :: Show Point where
  show = showPoint

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

-- Group 3, Exercise 7's required Eq implementation
instance eqPoint :: Eq Point where
  eq (Point {x: x1, y: y1}) (Point {x: x2, y: y2}) = x1 == x2
                                                  && y1 == y2

-- should I just use some generic? --in the future I definitely will.
instance eqShape :: Eq Shape where
  eq (Circle    p1    r1)
     (Circle    p2    r2) = p1 == p2
                         && r1 == r2
  eq (Rectangle p1 w1 h1)
     (Rectangle p2 w2 h2) = p1 == p2
                         && w1 == w2
                         && h1 == h2
  eq (Line      p1    q1)
     (Line      p2    q2) = p1 == p2
                         && q1 == q2

  eq (Text      p1    s1)
     (Text      p2    s2) = p1 == p2
                         && s1 == s2
  eq _ _ = false

-- generic version
--instance eqShape :: Eq Shape where
--  eq = genericEq
-- Look at how much EASIER that is!!!!

-- Group 3, Exercise 8's required Ord instance
pointMagnitude :: Point -> Number
pointMagnitude (Point {x,y}) = sqrt $ x*x + y*y

compareDim :: (Point -> Number) -> Point -> Point -> Ordering
compareDim f p1 p2 = compare (f p1) (f p2)

compareMagnitude :: Point -> Point -> Ordering
compareMagnitude = compareDim pointMagnitude

compareX :: Point -> Point -> Ordering
compareX = compareDim (\(Point dims) -> dims.x)

compareY :: Point -> Point -> Ordering
compareY = compareDim (\(Point dims) -> dims.y)

instance ordPoint :: Ord Point where
  compare p1 p2 = case (compareMagnitude p1 p2) of
                      GT -> GT
                      LT -> LT
                      EQ -> case (compareX p1 p2) of
                                GT -> GT
                                LT -> LT
                                EQ -> (compareY p1 p2)

-- I clearly misunderstand this with the amount of work I'm doing
instance ordShape :: Ord Shape where
  -- ok now that the boilerplate is done (is there a way of automating that?)
  compare (Circle p1 r1)
          (Circle p2 r2) = case (compare r1 r2) of
                               GT -> GT
                               LT -> LT
                               EQ -> (compare p1 p2)

  compare (Rectangle p1 w1 h1)
          (Rectangle p2 w2 h2) = case (compare whp1 whp2) of
                                     GT -> GT
                                     LT -> LT
                                     EQ -> (compare p1 p2)
                                 where whp1 = (Point {x: w1, y: h1})
                                       whp2 = (Point {x: w2, y: h2})

  compare (Line (Point pa) (Point pb))
          (Line (Point pc) (Point pd)) =
            let deltaPt :: { x :: Number, y :: Number }
                        -> { x :: Number, y :: Number }
                        -> Point
                deltaPt p1 p2 = Point {x: p2.x-p1.x, y: p2.x-p1.x}
                delta1 = deltaPt pa pb
                delta2 = deltaPt pc pd
             in case (compare delta1 delta2) of
                    GT -> GT
                    LT -> LT
                    EQ -> case (compare pa pc) of
                              GT -> GT
                              LT -> LT
                              EQ -> (compare pb pd)

  compare (Text p1 s1)
          (Text p2 s2) = case (compare s1 s2) of
                             GT -> GT
                             LT -> LT
                             EQ -> compare p1 p2

  compare (Text _ _)        _ = LT
  compare  _        (Text _ _)= GT
  compare (Line _ _)        _ = LT
  compare _         (Line _ _)= GT
  compare (Rectangle _ _ _) _ = LT
  compare _  (Rectangle _ _ _)= GT
  compare (Circle _ _)      _ = GT
  compare _       (Circle _ _)= LT


-- all of what I wrote up there is neat or whatever, but I should've just done:
--derive instance ordPoint :: Ord Point
--derive instance ordShape :: Ord Shape
-- as per the document we referenced for the chapter:
--For example, if you you'd like to be able to remove duplicates from an array
--of an ADT using nub, you need an Eq and Ord instance. Rather than writing 
--these manually, let the compiler do the work.
--https://github.com/purescript/documentation/blob/master/guides/Type-Class-Deriving.md
