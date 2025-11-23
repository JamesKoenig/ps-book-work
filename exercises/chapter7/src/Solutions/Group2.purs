module Solutions.Group2 where

import Prelude
import Data.String.Regex (Regex)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)


-- 1. (Easy) Write a regular expression `stateRegex :: Regex` to check that a 
--           string only contains two alphanumeric characters.
stateRegex :: Regex
stateRegex = unsafeRegex "^[a-zA-Z]{2}$" noFlags

-- 2. (Medium) Write a regular expression `nonEmptyRegex :: Regex` to check that
--             a string is not entirely whitespace.

-- I wanted to use `regex` on this but that returns `Either String Regex`
nonEmptyRegex :: Regex
nonEmptyRegex = unsafeRegex "\\S+" noFlags
