module Solutions.Group1 ( concatenateFiles
                        , concatenateMany
                        , countCharacters
                        ) where
import Prelude
import Effect.Aff ( Aff
                  , attempt
                  )
import Node.Encoding (Encoding(..))
import Node.FS.Aff ( readTextFile
                   , writeTextFile
                   )
import Node.Path ( FilePath
                 )
import Data.Traversable (traverse)
import Data.Foldable (foldr)
import Data.Either (Either)
import Effect.Exception (Error)
import Data.String (length)

--Exercise 1. (Easy) Write a function `concatenateFiles` function that 
--                   concatenates two files
concatenateFiles :: FilePath -> FilePath -> FilePath -> Aff Unit
concatenateFiles inFirst inSecond outPath = do
  first_data  <- readTextFile UTF8 inFirst
  second_data <- readTextFile UTF8 inSecond

  writeTextFile UTF8 outPath (first_data <> second_data)

concatenateMany :: Array FilePath -> FilePath -> Aff Unit
concatenateMany inPaths outPath = do
  fileContents <- traverse (readTextFile UTF8) inPaths

  let toWrite = foldr (<>) "" fileContents

  writeTextFile UTF8 outPath toWrite

countCharacters :: FilePath -> Aff (Either Error Int)
countCharacters filePath = attempt go
  where go = do
          contents <- readTextFile UTF8 filePath
          pure <<< length $ contents
