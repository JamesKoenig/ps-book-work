module Solutions.Group5 where

import Prelude

import Data.Path
import Data.Maybe (Maybe)
import Data.Array ((:)
                  ,filter
                  ,head
                  ,fromFoldable
                  ,length
                  ,tail
                  )
import Control.Alternative (guard)
import Data.Ord (Ordering(..)
                ,compare
                )
import Data.Foldable (foldl)

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

-- 2. (Medium) Write a function `whereIs` to search for a file by name.  The
--             funtion should return a value of type `Maybe Path`
whereIs :: Path -> String -> Maybe Path
whereIs path fName = head $ finder
  where finder :: Array Path
        finder = do
           parent <- allFiles path
           child  <- ls parent
           guard $ (filename parent) <> fName == filename child
           pure parent

-- 3. (Difficult) Write a function `largestSmallest` which takes a `Path` and
--                returns an array containing the single largest and single
--                smallest files in a directory

-- TODO FIXME redo this so that it doesn't cause me an undue amount of anxiety
largestSmallest :: Path -> Array Path
largestSmallest path =
  let files :: Array Path
      files = onlyFiles path
      largerFile :: Path -> Path -> Path
      largerFile (File namel sizel) (File namer sizer) =
        case compare sizel sizer of
            LT -> (File namer sizer)
            _  -> (File namel sizel)
      largerFile _ _ = (File "how did you get here" (-1))

-- WHY WON'T THIS WORK!??!?!?!
--      smallerFile :: Path -> Path -> Path
--      smallerFile = flip largerFile

-- IS IT THE EQ? THIS IS LITERALLY JUST THE largerFile FUNCTION FLIPPED THO?!
      smallerFile :: Path -> Path -> Path
      smallerFile (File namel sizel) (File namer sizer) =
       case compare sizel sizer of
           GT -> (File namer sizer)
           _  -> (File namel sizel)
      smallerFile _ _ = (File "how did you get here" (-1))

      -- takes a comparison function and returns the result
      minSupp :: (Path -> Path -> Path) -> Array Path
      minSupp compare | length files == 0 = []
                      | otherwise         = fromFoldable do
                                                  hd <- head files
                                                  tl <- tail files
                                                  pure $ foldl (compare) hd tl
      largest :: Array Path
      largest  = minSupp largerFile
      smallest :: Array Path
      smallest = minSupp smallerFile
   in
     case length files of
         0 -> []
         1 -> files
         _ -> largest <> smallest
