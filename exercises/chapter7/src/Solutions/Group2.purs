module Solutions.Group2 where

import Prelude
import Data.String.Regex (Regex)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Validation.Semigroup (V)
import Data.AddressBook (Address
                        ,address
                        )
import Data.AddressBook.Validation (matches
                                   ,Errors
                                   )

-- 1. (Easy) Write a regular expression `stateRegex :: Regex` to check that a 
--           string only contains two alphanumeric characters.
stateRegex :: Regex
stateRegex = unsafeRegex "^[a-zA-Z]{2}$" noFlags

-- 2. (Medium) Write a regular expression `nonEmptyRegex :: Regex` to check that
--             a string is not entirely whitespace.

-- I wanted to use `regex` on this but that returns `Either String Regex`
nonEmptyRegex :: Regex
nonEmptyRegex = unsafeRegex "\\S+" noFlags

-- 3. (Medium) Write a function `validateAddressImproved` that is similar to
--             `validateAddress` but uses the above `stateRegex` to validate
--             the `state` field and `nonEmptyRegex` to validate the `street`
--             and `city` fields.

-- using ado notation instead of <$> <*> <*> <*>
validateAddressImproved :: Address -> V Errors Address
validateAddressImproved a = ado
  street <- matches "Street" nonEmptyRegex  a.street
  city   <- matches "City"   nonEmptyRegex  a.city
  state  <- matches "State"  stateRegex     a.state
  in address street city state
