module Solutions.Group3 ( concatenateManyParallel
                        , getWithTimeout
                        ) where

import Prelude
import Node.Path (FilePath)
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
import Data.Foldable (fold)

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
