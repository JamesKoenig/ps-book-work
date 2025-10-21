module Solutions.AddressBook where

--I'm avoiding Prelude because I enjoy making things more difficult
--  (i.e. familiarizing myself with pursuit search more)
import Data.Function (($))
import Data.Eq ((==))
import Data.HeytingAlgebra ((&&)
                           ,not
                           )
import Control.Semigroupoid ((<<<))
import Data.List (filter
                 ,head
                 ,null
                 ,(:)
                 ,List(..)
                 )
import Data.AddressBook (AddressBook
                        ,Entry
                        ,findEntry
                        )
import Data.Maybe (Maybe
                  ,isJust
                  )

-- I went straight for ex3's constraints here b/c it seemed cool!
findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet street = head <<< filter ((_==street) <<< (_.address.street))

isInBook :: String -> String -> AddressBook -> Boolean
isInBook firstName lastName = isJust <<< (findEntry firstName lastName)
--isInBook = isInBook'
--isInBook = isInBook''

isInBook' :: String -> String -> AddressBook -> Boolean
isInBook' first last =
  let filterEntry :: Entry -> Boolean
      filterEntry { firstName, lastName } =  first == firstName
                                          && last  == lastName
   in
  not <<< null <<< filter filterEntry

isInBook'' :: String -> String -> AddressBook -> Boolean
isInBook'' _     _    Nil = false
isInBook'' first last ({firstName, lastName} : _ ) |  first == firstName
                                                    && last == lastName   =
                                                      true
isInBook'' first last ( _ : xs) = isInBook'' first last xs

