module Solutions.Group1 ( concatenateFiles
                        ) where

import Prelude
import Effect.Aff ( Aff
                  )
import Effect (Effect)
import Node.Encoding (Encoding(..))
import Node.FS.Aff ( readTextFile
                   , writeTextFile
                   )
import Node.Path ( FilePath
                 )

--Exercise 1. (Easy) Write a function `concatenateFiles` function that 
--                   concatenates two files
concatenateFiles :: FilePath -> FilePath -> FilePath -> Aff Unit
concatenateFiles inFirst inSecond outPath = do
  first_data  <- readTextFile UTF8 inFirst
  second_data <- readTextFile UTF8 inSecond

  writeTextFile UTF8 outPath (first_data <> second_data)
