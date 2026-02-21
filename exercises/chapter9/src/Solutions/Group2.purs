module Solutions.Group2 (writeGet) where

import Prelude
import Node.Path (FilePath)
import Effect.Aff (Aff)
import Affjax.Node as AN
import Affjax.ResponseFormat as ResponseFormat
import Data.Either (Either(..))
import Node.FS.Aff (writeTextFile)
import Node.Encoding (Encoding(UTF8))

-- 1. (Easy) Write a function `writeGet` which makes an HTTP `GET` request
--           to a provided URL, and writes the response body to a file.

writeGet :: String -> FilePath -> Aff Unit
writeGet url outFile = do
  res <- AN.get ResponseFormat.string url
  case res of
      Left _ -> pure unit
      Right response -> do
         writeTextFile UTF8 outFile response.body
