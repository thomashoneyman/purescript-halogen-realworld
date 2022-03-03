-- | Where there are forms, there is inevitably validation. We often need to ensure that user
-- | input passes a few checks before allowing them to submit a form to the server. This module
-- | provides generic validation that can be used in all sorts of different forms, like validating
-- | an input string is long enough, that a username is well-formed, or that a required field is
-- | filled in.
module Conduit.Form.Validation where

import Prelude

import Conduit.Data.Avatar (Avatar)
import Conduit.Data.Avatar as Avatar
import Conduit.Data.Email (Email(..))
import Conduit.Data.Username (Username)
import Conduit.Data.Username as Username
import Data.Either (Either(..), note)
import Data.Maybe (Maybe(..))
import Data.String as String

-- | This short list of errors represent the only ways in which validation could have failed
-- | on a given field. As our application grows, we might revise this type so that each form
-- | and field has its own small subset of possible errors using extensible sum types (variants).
-- | For now, this small type covers all potential validation errors.
data FormError
  = Required
  | TooShort
  | TooLong
  | InvalidEmail
  | InvalidUsername
  | InvalidAvatar

-- | When a field has failed to pass validation, it will produce an error instead of a success
-- | value. But we don't want to show our users something they'd never see in usual English like
-- | `TooLong`. Instead, we'll produce human-readable text for each possible error.
errorToString :: FormError -> String
errorToString = case _ of
  Required -> "This field is required."
  TooShort -> "Not enough characters entered"
  TooLong -> "Too many characters entered"
  InvalidEmail -> "Invalid email address"
  InvalidUsername -> "Invalid username"
  InvalidAvatar -> "Invalid image URL"

-- | We're using Formless, a form library for Halogen. It abstracts away the mechanics of updating
-- | fields, maintaining form state, applying validation, and more. Validators in Formless are
-- | composable and offer more powerful features than usual applicative validation, namely the ability
-- | to refer to other fields in the form and to perform effects like asynchronous server-side
-- | validation.
-- |
-- | For a more structured introduction to Formless and validation, consider reading the official guide:
-- | https://github.com/thomashoneyman/purescript-halogen-formless
-- |

-- | Ensure the input is not empty by comparing the input value to the 'empty'
-- | value represented by the `Monoid` class.
required :: forall a. Eq a => Monoid a => a -> Either FormError a
required = check (_ /= mempty) Required

-- | Ensure that an input string is longer than the provided lower limit.
minLength :: Int -> String -> Either FormError String
minLength n = check (\str -> String.length str > n) TooShort

-- | Ensure that an input string is shorter than the provided upper limit.
maxLength :: Int -> String -> Either FormError String
maxLength n = check (\str -> String.length str <= n) TooLong

-- | Ensure that an input string is a valid email address, using a fairly naive
-- | requirement that it at least includes the `@` symbol.
emailFormat :: String -> Either FormError Email
emailFormat = map Email <<< check (String.contains (String.Pattern "@")) InvalidEmail

-- | Ensure that an input string is a valid username. Usernames in Conduit use
-- | the smart constructor pattern, so we can't construct a username directly --
-- | we'll need to defer to the `parse` helper function exported by
-- | `Conduit.Data.Username`. Since that function returns a `Maybe` value, we'll
-- | use the `note` helper from `Data.Either` to turn the `Nothing` case into
-- | an error.
usernameFormat :: String -> Either FormError Username
usernameFormat = note InvalidUsername <<< Username.parse

-- | Our avatar validator follows the same pattern, validating and transforming
-- | an input string into an `Avatar`.
avatarFormat :: String -> Either FormError Avatar
avatarFormat = note InvalidAvatar <<< Avatar.parse

-- | A small helper function for writing validation functions that rely on a
-- | true/false predicate.
check :: forall a. (a -> Boolean) -> FormError -> a -> Either FormError a
check f err a
  | f a = Right a
  | otherwise = Left err

-- | Sometimes we'd like to validate an input only if it isn't empty. This is useful for optional
-- | fields: if you've provided a value, we'll validate it, but if you haven't, then you should
-- | still be able to submit the form without error. For instance, we might allow a user to
-- | optionally provide an email address, but if they do, it must be valid.
-- |
-- | This helper function lets us transform a set of validation rules so that they only apply when
-- | the input is not empty. It isn't used in this module, but is used in the various forms.
toOptional
  :: forall a b
   . Monoid a
  => Eq a
  => (a -> Either FormError b)
  -> (a -> Either FormError (Maybe b))
toOptional k = \value ->
  if value == mempty then
    Right Nothing
  else
    map Just $ k value
