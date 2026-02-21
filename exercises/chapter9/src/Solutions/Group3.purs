module Solutions.Group3 ( concatenateManyParallel
                        , getWithTimeout
                        ) where

import Prelude
import Node.Path (FilePath)
import Effect.Aff (Aff)
import Control.Parallel (parTraverse)
import Node.FS.Aff (readTextFile, writeTextFile)
import Node.Encoding (Encoding(UTF8))
import Data.Foldable (foldr)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.HTTP.Method (Method(GET))
import Affjax.Node as AN
import Data.Time.Duration (Milliseconds(..))
import Affjax.ResponseFormat (string)

concatenateManyParallel :: Array FilePath -> FilePath -> Aff Unit
concatenateManyParallel inPaths outPath = do
  fileContents <- parTraverse (readTextFile UTF8) inPaths

  let toWrite = foldr (<>) "" fileContents

  writeTextFile UTF8 outPath toWrite

getWithTimeout :: Number -> String -> Aff (Maybe String)
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
