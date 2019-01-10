-- | Where there are forms, there is inevitably validation. We often need to ensure that user
-- | input passes a few checks before allowing them to submit a form to the server. This module
-- | provides generic validation that can be used in all sorts of different forms, like validating 
-- | an input string is long enough, that a username is well-formed, or that a required field is 
-- | filled in.
-- |
-- | For more details on how to write validators in Formless, see the official guide:
-- | https://github.com/thomashoneyman/purescript-halogen-formless/blob/v0.5.2/readme.md
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
import Formless as F

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
-- | https://github.com/thomashoneyman/purescript-halogen-formless/blob/v0.5.2/readme.md
-- |
-- | In order to validate a particular field, we need to give Formless a value of type `Validation`, 
-- | which takes several type parameters:
-- |
-- | `form`, which represents the fields of the particular form the validation is meant for. This lets
-- |   you do things like compare the value of one field to the value of another, checked at 
-- |   compile-time. Unless your validation relies on a different field from the one being validated,
-- |   you'll usually leave this parameter open.
-- | `m`, which represents which monad the Formless is being run in. This lets you perform effectful
-- |   computations like asynchronously runnning some server-side validation. Once again, unless you
-- |   need a specific monadic ability, this is usually left open.
-- | `e`, which represents the possible error type which can result from the validator. For Conduit
-- |   we'll always fill this in with our custom `FormError` type.
-- | `i`, which represents the input type being validated. For a validator that operates on strings,
-- |   this would be `String`, for a validator that operates on numbers, this would be `Number`, and
-- |   so on. This is usually filled in with a concrete type or a constraint like `Monoid`.
-- | `o`, which represents the parsed output that will result from successful validation. If your 
-- |   validator checks whether a username is valid, it might have an input type of `String` and  
-- |   an output type of `Username`. This is usually filled in with a concrete type, or asserted 
-- |   to be the same as the input type.
-- |
-- | For the most part, the generic validation functions we'll write just need to transform some
-- | input into some output, possibly failing, without the need to refer to any other values in the
-- | form or perform effects. When you have a simple function of this form...
-- |
-- | ```purescript
-- | check:: forall i e o. i -> Either e o
-- | ```
-- |
-- | ...then you can use the `hoistFnE_` helper from Formless to automatically turn it into the 
-- | correct `Validation` type. We'll use this helper to write several simple, pure validators and
-- | then make them compatible with Formless.

-- | The first validator we'll write verifies that the input is not empty. The same validator can
-- | apply to any input value that is a monoid, as the `Monoid` type class represents 'emptiness'
-- | with the `mempty` value. We'll just check whether the input is the empty value.
required :: ∀ form m a. Eq a => Monoid a => Monad m => F.Validation form m FormError a a
required = F.hoistFnE_ $ cond (_ /= mempty) Required

-- | This validator ensures that an input string is longer than the provided lower limit. 
minLength :: ∀ form m. Monad m => Int -> F.Validation form m FormError String String
minLength n = F.hoistFnE_ $ cond (\str -> String.length str > n) TooShort

-- | This validator ensures that an input string is shorter than the provided upper limit. 
maxLength :: ∀ form m. Monad m => Int -> F.Validation form m FormError String String
maxLength n = F.hoistFnE_ $ cond (\str -> String.length str <= n) TooLong

-- | This validator ensures that an input string is a valid email address, using a fairly naive
-- | requirement that it at least includes the `@` symbol. This is our first example of a validator 
-- | that returns a different output value than its input value.
emailFormat :: ∀ form m. Monad m => F.Validation form m FormError String Email
emailFormat = F.hoistFnE_ $ map Email <<< cond (String.contains (String.Pattern "@")) InvalidEmail

-- | This validator ensures that an input string is a valid username. Usernames in Conduit use the
-- | smart constructor pattern, so we can't construct a username directly -- we'll need to defer 
-- | to the `parse` helper function exported by `Conduit.Data.Username`. Since that function returns
-- | a `Maybe` value, we'll use the `note` helper from `Data.Either` to turn the `Nothing` case into
-- | an error.
usernameFormat :: ∀ form m. Monad m => F.Validation form m FormError String Username
usernameFormat = F.hoistFnE_ $ note InvalidUsername <<< Username.parse

-- | Our avatar validator follows the same pattern, validating and transforming an input string into
-- | an `Avatar`. 
avatarFormat :: ∀ form m. Monad m => F.Validation form m FormError String Avatar
avatarFormat = F.hoistFnE_ $ note InvalidAvatar <<< Avatar.parse 

-- Utilities

-- | Validation often relies on a true/false function (a predicate), where `true` should return the
-- | input value and `false` should return the correct error. This pattern happens often enough that
-- | I've created a small helper, `cond`, which abstracts the pattern.
cond :: forall a. (a -> Boolean) -> FormError -> a -> Either FormError a
cond f err a = if f a then pure a else Left err 

-- | Sometimes we'd like to validate an input only if it isn't empty. This is useful for optional 
-- | fields: if you've provided a value, we'll validate it, but if you haven't, then you should 
-- | still be able to submit the form without error. For instance, we might allow a user to
-- | optionally provide an email address, but if they do, it must be valid.
-- |
-- | This helper function lets us transform a set of validation rules so that they only apply when 
-- | the input is not empty. It isn't used in this module, but is used in the various forms. 
toOptional :: ∀ form m a b
   . Monoid a 
  => Eq a
  => Monad m 
  => F.Validation form m FormError a b
  -> F.Validation form m FormError a (Maybe b)
toOptional v = F.Validation \form val -> 
  case val == mempty of
    true -> pure (pure Nothing)
    _ -> (map <<< map) Just (F.runValidation v form val)
