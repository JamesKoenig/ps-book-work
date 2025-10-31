module Solutions.Group1 where

import Prelude

newtype Point = Point { x :: Number, y :: Number }

-- 1. (Easy) Define a `Show` instance for `Point`. Match the same output as the
--           `showPoint` function from the previous chapter.  _Note_: Point is
--           now a `newtype` (instead of a `type` synonym), which allows us to
--           customize how to `show` it.  Otherwise we'd be stuck with the
--           default `Show` instance for records.

-- Several notes:
--    1. The book clearly says
--        " The output of show should be a string that you can
--          paste back into the repl (or .purs file) to recreate
--          the item being shown. "
--       and the function as wanted does not qualify for that definition
--    2. Chapters 4 and 5 were swapped, so instad of 'previous chapter' this
--       exercise should say 'chapter 4' since `Data.Picture` and `Point` with
--       it are from the 'Pattern Matching' chapter.
showPoint :: Point -> String
showPoint (Point { x, y }) =
  "(" <> show x <> ", " <> show y <> ")"

instance pointShow :: Show Point where
  show = showPoint

