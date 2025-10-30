module Solutions.Group5 where

import Prelude

import Data.Path
import Data.Array ((:)
                  ,filter
                  )

--I originally wanted to redo this with List somehow, but I couldn't figure out
--  how to deal with the type constraints of fmap and bind... maybe some other
--  time? or maybe with a kind of data type or FFI? TODO
allFiles :: Path -> Array Path
allFiles file = file : do
  child <- ls file
  allFiles child

-- 1. (Easy) Write a function `onlyFiles` which returns all _files_ (not
--           directories) in all subdirectories of a directory.
onlyFiles :: Path -> Array Path
onlyFiles path = filter (not <<< isDirectory) (allFiles path)
