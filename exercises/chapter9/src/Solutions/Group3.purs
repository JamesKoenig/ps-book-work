module Solutions.Group3 ( concatenateManyParallel
                        , getWithTimeout
                        , recurseFiles
                        ) where

import Prelude
import Node.Path (FilePath)
import Node.Path as Path
import Effect.Aff (Aff)
import Control.Parallel (parTraverse)
import Node.FS.Aff (readTextFile, writeTextFile)
import Node.Encoding (Encoding(UTF8))
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.HTTP.Method (Method(GET))
import Affjax.Node as AN
import Data.Time.Duration (Milliseconds(..))
import Affjax.ResponseFormat (string)
import Data.String.Regex (split)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.String.Regex.Flags (noFlags)
import Data.Foldable (fold)
import Data.Array (filter)

-- (Easy) Write a `concatenateManyParallel` function with the same signature
--        as the earliere `concatenateMany` function but reads all input files
--        in parallel
concatenateManyParallel :: Array FilePath -> FilePath -> Aff Unit
concatenateManyParallel inPaths outPath = do
  fileContents <- parTraverse (readTextFile UTF8) inPaths

  let toWrite = fold fileContents

  writeTextFile UTF8 outPath toWrite

-- (Medium) Write a:
getWithTimeout :: Number -> String -> Aff (Maybe String)
--          function which makes an HTTP `GET` request at the provided URL and
--          returns either `Nothing` if the request takes longer than the
--          provided timeout, or the string respnose if the request succeeds
--          before the timeout elapses.
getWithTimeout timeout url = do
  let ms = Milliseconds timeout
      req = AN.defaultRequest { url = url
                              , method = Left GET
                              , timeout = Just ms
                              , responseFormat = string
                              }

  result <- AN.request req

  case result of
      Left _ -> pure Nothing
      Right response -> pure <<< Just $ response.body

-- (Difficult) Write a `recurseFiles` function that takes a "root" file and
--             returns an array of all paths listed in that file (and listed in
--             the listed files too), Read the listed files in parallel. Paths
--             are relative to the directory of the file they appear in.

fileLines :: String -> Array String
fileLines = split $ unsafeRegex "\n" noFlags

readAndPrepend :: FilePath -> Aff (Array FilePath)
readAndPrepend file = do
   content <- readTextFile UTF8 file
   let lines = filter (_/="") $ fileLines content :: Array FilePath
       here  = Path.dirname file :: FilePath
       addContext :: FilePath -> FilePath
       addContext x = Path.concat [here,x]
       paths = addContext <$> lines

   pure paths

recurseFilesStep :: Array FilePath -> Aff (Array FilePath)
recurseFilesStep files =
    fold <$> parTraverse readAndPrepend files

recurseFiles :: FilePath -> Aff (Array FilePath)
recurseFiles root = go [root] [root]
  where go :: Array FilePath -> Array FilePath -> Aff (Array FilePath)
        go acc []   = pure acc
        go acc todo = do
          results <- recurseFilesStep todo
          go (acc <> results) results

-- Whiteboarding:
-- e.g.
--  file 1 is at location foo/bar/root.txt
--  file 1 contains the line 'a.txt'
--  file 2 is at location foo/bar/a.txt
--  file 2 is empty.
-- to start put foo/bar/root.txt at the top of a stack
-- loop*:                (* Iterative, move to parallelization once understood)
--    - remove the top element from the stack               (foo/bar/root.txt)
--    - remember its dir path                               (foo/bar)
--    - read its contents                                   ("a.txt")
--    - split those contents by lines                       (["a.txt"])
--    - for each of the lines:
--      - if the line isn't empty:
--        - concat the dir path to the line                 ("foo/bar/a.txt")
--        - push the result onto a new stack (not necessary for map)
--    - (dfs) push for-stack onto loop-stack
--    OR
--    - (bfs) append the for-stack after the loop-stack
