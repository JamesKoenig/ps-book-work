module Solutions.AddressBook where

--I'm avoiding Prelude because I enjoy making things more difficult
--  (i.e. familiarizing myself with pursuit search more)
import Data.Function (($))
import Data.Eq ((==))
import Control.Semigroupoid ((<<<))
import Data.List (filter
                 ,head
                 )
import Data.AddressBook (AddressBook
                        ,Entry
                        )
import Data.Maybe (Maybe)

-- I went straight for ex3's constraints here b/c it seemed cool!
findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet street = head <<< filter ((_==street) <<< (_.address.street))
