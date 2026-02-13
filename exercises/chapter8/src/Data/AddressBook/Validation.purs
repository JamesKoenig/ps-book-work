module Data.AddressBook.Validation where

import Prelude

import Data.AddressBook ( Address
                        , Person
                        , PhoneNumber
                        , address
                        , person
                        , phoneNumber
                        , PhoneType
                        )
import Data.Either (Either)
import Data.String (length)
import Data.String.Regex (Regex, test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (traverse)
import Data.Validation.Semigroup (V, invalid, toEither)
import Data.Maybe (Maybe(..))

data FailedField = FirstNameField
                 | LastNameField
                 | StreetField
                 | CityField
                 | StateField
                 | PhoneField (Maybe PhoneType)

instance Show FailedField where
  show FirstNameField = "First Name"
  show LastNameField  = "Last Name"
  show StreetField    = "Street"
  show CityField      = "City"
  show StateField     = "State"

  show (PhoneField (Just t)) = show t
  show (PhoneField Nothing)  = "Phone Numbers"

data ValidationError = ValidationError String FailedField
type Errors = Array ValidationError

vError :: FailedField -> String -> ValidationError
vError field reason = let errString = "Field '"
                                   <> show field
                                   <> reason
                      in ValidationError errString field

nonEmpty :: FailedField -> String -> V Errors String
nonEmpty field ""     = invalid [ vError field "' cannot be empty" ]
nonEmpty _     value  = pure value

validatePhoneNumbers :: Array PhoneNumber -> V Errors (Array PhoneNumber)
validatePhoneNumbers []      =
  invalid [ vError (PhoneField Nothing) "' must contain at least one value" ]
validatePhoneNumbers phones  =
  traverse validatePhoneNumber phones

lengthIs :: FailedField -> Int -> String -> V Errors String
lengthIs field len value | length value /= len =
  invalid [ vError field $ "' must have length " <> show len ]
lengthIs _     _   value = pure value

phoneNumberRegex :: Regex
phoneNumberRegex = unsafeRegex "^\\d{3}-\\d{3}-\\d{4}$" noFlags

matches :: FailedField -> Regex -> String -> V Errors String
matches _     regex value | test regex value
                          = pure value
matches field _     _     = invalid [ vError
                                        field
                                        "' did not match the required format"
                                    ]

validateAddress :: Address -> V Errors Address
validateAddress a =
  address <$> nonEmpty StreetField  a.street
          <*> nonEmpty CityField    a.city
          <*> lengthIs StateField 2 a.state

validatePhoneNumber :: PhoneNumber -> V Errors PhoneNumber
validatePhoneNumber pn =
  let phoneType  = pn."type"
      phoneField = PhoneField $ Just phoneType
  in phoneNumber <$> pure phoneType
                 <*> matches phoneField phoneNumberRegex pn.number

validatePerson :: Person -> V Errors Person
validatePerson p =
  person <$> nonEmpty FirstNameField p.firstName
         <*> nonEmpty LastNameField  p.lastName
         <*> validateAddress p.homeAddress
         <*> validatePhoneNumbers p.phones

validatePerson' :: Person -> Either Errors Person
validatePerson' p = toEither $ validatePerson p
