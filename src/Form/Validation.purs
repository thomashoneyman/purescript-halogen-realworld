module Conduit.Form.Validation where

import Prelude

import Conduit.Data.Email (Email(..))
import Conduit.Data.Username (Username)
import Conduit.Data.Username as Username
import Data.Either (Either(..), note)
import Data.String as String
import Formless as F

data FormError
  = Required
  | TooShort
  | TooLong
  | InvalidEmail
  | InvalidUsername

errorToString :: FormError -> String
errorToString = case _ of
  Required -> "This field is required."
  TooShort -> "Not enough characters entered"
  TooLong -> "Too many characters entered"
  InvalidEmail -> "Invalid email address"
  InvalidUsername -> "Invalid username"

required :: ∀ form m a. Eq a => Monoid a => Monad m => F.Validation form m FormError a a
required = F.hoistFnE_ $ cond (_ /= mempty) Required

minLength :: ∀ form m. Monad m => Int -> F.Validation form m FormError String String
minLength n = F.hoistFnE_ $ cond (\str -> String.length str > n) TooShort

maxLength :: ∀ form m. Monad m => Int -> F.Validation form m FormError String String
maxLength n = F.hoistFnE_ $ cond (\str -> String.length str <= n) TooLong

emailFormat :: ∀ form m. Monad m => F.Validation form m FormError String Email
emailFormat = F.hoistFnE_ (map Email <<< cond (String.contains (String.Pattern "@")) InvalidEmail)

usernameFormat :: ∀ form m. Monad m => F.Validation form m FormError String Username
usernameFormat = F.hoistFnE_ $ Username.parse >>> note InvalidUsername

-- Utilities

cond :: forall a. (a -> Boolean) -> FormError -> a -> Either FormError a
cond f err a = if f a then pure a else Left err 