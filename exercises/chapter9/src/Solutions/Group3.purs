module Solutions.Group3 ( concatenateManyParallel
                        ) where

import Prelude
import Node.Path (FilePath)
import Effect.Aff (Aff)
import Control.Parallel (parTraverse)
import Node.FS.Aff (readTextFile, writeTextFile)
import Node.Encoding (Encoding(UTF8))
import Data.Foldable (foldr)

concatenateManyParallel :: Array FilePath -> FilePath -> Aff Unit
concatenateManyParallel inPaths outPath = do
  fileContents <- parTraverse (readTextFile UTF8) inPaths

  let toWrite = foldr (<>) "" fileContents

  writeTextFile UTF8 outPath toWrite
