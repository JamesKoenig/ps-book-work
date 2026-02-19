module Main where

import Prelude

import Data.AddressBook (PhoneNumber, examplePerson)
import Data.AddressBook.Validation ( Errors
                                   , validatePerson'
                                   , ValidationError(..)
                                   , FailedField(..)
                                   )
import Data.Array ( mapWithIndex
                  , updateAt
                  , filter
                  )
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)
import React.Basic.DOM as D
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler)
import React.Basic.Hooks (ReactComponent, element, reactComponent, useState)
import React.Basic.Hooks as R
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

-- Note that there's a Purty formatting bug that
-- adds an unwanted blank line
-- https://gitlab.com/joneshf/purty/issues/77
renderError :: ValidationError -> R.JSX
renderError (ValidationError err _)
  = D.li { className: "alert alert-danger row"
         , children:  [ D.text err ]
         }

renderValidationErrors :: Errors -> Array R.JSX
renderValidationErrors [] = []
renderValidationErrors xs =
    [ D.div_ [ D.ul_ (map renderError xs) ] ]

type FormProp = { name        :: String
                , placeholder :: String
                , value       :: String
                , setValue    :: String -> Effect Unit
                , errors      :: Errors
                }

-- assumes plaeholder == name
quickFormProp ::
  Errors -> String -> String -> (String -> Effect Unit) -> FormProp
quickFormProp errors name value setValue =
  { name
  , placeholder: name
  , value
  , setValue
  , errors
  }

-- Helper function to render a single form field with an
-- event handler to update
formField :: FormProp -> R.JSX
formField props =
  let { name, placeholder, value, setValue, errors } = props
  in D.div
    { className: "form-group row"
    , children:
        [ D.label
            { className: "col-sm col-form-label"
            , htmlFor: name
            , children: [ D.text name ]
            }
        , D.div
            { className: "col-sm"
            , children:
                [ D.input
                    { className: "form-control"
                    , id: name
                    , placeholder
                    , value
                    , onChange:
                        let
                          handleValue :: Maybe String -> Effect Unit
                          handleValue (Just v) = setValue v
                          handleValue Nothing  = pure unit
                        in
                          handler targetValue handleValue
                    }
                ]
            }
        ]
        <> renderValidationErrors errors
    }

mkAddressBookApp :: Effect (ReactComponent {})
mkAddressBookApp =
  -- incoming \props are unused
  reactComponent "AddressBookApp" \_props -> R.do
    -- `useState` takes a default initial value and returns the
    -- current value and a way to update the value.
    -- Consult react-hooks docs for a more detailed explanation of `useState`.
    Tuple person setPerson <- useState examplePerson
    let
      errors = case validatePerson' person of
        Left  e -> e
        Right _ -> []

      phoneErrors = filter isPhone errors
        where isPhone (ValidationError _ (PhoneField _)) = true
              isPhone _                                  = false

      filterErrors :: FailedField ->  Errors
      filterErrors field = filter hasField errors
        where hasField (ValidationError _ failedField) = failedField == field

      -- helper-function to return array unchanged instead of Nothing if index is out of bounds
      updateAt' :: forall a. Int -> a -> Array a -> Array a
      updateAt' i x xs = fromMaybe xs (updateAt i x xs)

      -- helper-function to render a single phone number at a given index
      renderPhoneNumber :: Int -> PhoneNumber -> R.JSX
      renderPhoneNumber index phone =
        let setValue s =
              setPerson _ { phones = updateAt'
                            index
                            phone { number = s }
                            person.phones
                          }
            props = { name:        (show phone."type")
                    , placeholder: "XXX-XXX-XXXX"
                    , value:       phone.number
                    , setValue
                    , errors: []
                    }
        in formField props

      -- helper-function to render all phone numbers
      renderPhoneNumbers :: Array R.JSX
      renderPhoneNumbers = mapWithIndex renderPhoneNumber person.phones
    pure
      $ D.div
          { className: "container"
          , children:
              renderValidationErrors errors
                <> [ D.div
                      { className: "row"
                      , children:
                          [ D.form_
                              $ [ D.h3_ [ D.text "Basic Information" ]
                                , formField
                                  (quickFormProp
                                    (filterErrors FirstNameField)
                                    "First Name"
                                    person.firstName
                                    \s -> setPerson _ { firstName = s }
                                  )
                                , formField
                                  (quickFormProp
                                    (filterErrors LastNameField)
                                    "Last Name"
                                    person.lastName
                                    \s -> setPerson _ { lastName = s }
                                  )
                                , D.h3_ [ D.text "Address" ]
                                , formField
                                  (quickFormProp
                                    (filterErrors StreetField)
                                    "Street"
                                    person.homeAddress.street
                                    \s ->
                                      setPerson _ { homeAddress { street = s } }
                                  )
                                , formField
                                  (quickFormProp
                                    (filterErrors CityField)
                                    "City"
                                    person.homeAddress.city
                                    \s ->
                                      setPerson _ { homeAddress { city = s } }
                                  )
                                , formField
                                  (quickFormProp
                                    (filterErrors StateField)
                                    "State"
                                    person.homeAddress.state
                                    \s ->
                                      setPerson _ { homeAddress { state = s } }
                                  )
                                , D.h3_ [ D.text "Contact Information" ]
                                ]
                              <> renderValidationErrors phoneErrors
                              <> renderPhoneNumbers
                          ]
                      , key: "person-form"
                      }
                  ]
          }

main :: Effect Unit
main = do
  log "Rendering address book component"
  -- Get window object
  w <- window
  -- Get window's HTML document
  doc <- document w
  -- Get "container" element in HTML
  ctr <- getElementById "container" $ toNonElementParentNode doc
  case ctr of
    Nothing -> throw "Container element not found."
    Just c -> do
      -- Create AddressBook react component
      addressBookApp <- mkAddressBookApp
      let
        -- Create JSX node from react component. Pass-in empty props
        app = element addressBookApp {}
      -- Render AddressBook JSX node in DOM "container" element
      D.render app c
