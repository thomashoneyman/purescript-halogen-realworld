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

data FormError
  = Required
  | TooShort
  | TooLong
  | InvalidEmail
  | InvalidUsername
  | InvalidAvatar

errorToString :: FormError -> String
errorToString = case _ of
  Required -> "This field is required."
  TooShort -> "Not enough characters entered"
  TooLong -> "Too many characters entered"
  InvalidEmail -> "Invalid email address"
  InvalidUsername -> "Invalid username"
  InvalidAvatar -> "Invalid image URL"

-- General validation

required :: ∀ form m a. Eq a => Monoid a => Monad m => F.Validation form m FormError a a
required = F.hoistFnE_ $ cond (_ /= mempty) Required

-- Concrete validation

minLength :: ∀ form m. Monad m => Int -> F.Validation form m FormError String String
minLength n = F.hoistFnE_ $ cond (\str -> String.length str > n) TooShort

maxLength :: ∀ form m. Monad m => Int -> F.Validation form m FormError String String
maxLength n = F.hoistFnE_ $ cond (\str -> String.length str <= n) TooLong

-- Custom types

emailFormat :: ∀ form m. Monad m => F.Validation form m FormError String Email
emailFormat = F.hoistFnE_ (map Email <<< cond (String.contains (String.Pattern "@")) InvalidEmail)

usernameFormat :: ∀ form m. Monad m => F.Validation form m FormError String Username
usernameFormat = F.hoistFnE_ $ Username.parse >>> note InvalidUsername

avatarFormat :: ∀ form m. Monad m => F.Validation form m FormError String Avatar
avatarFormat = F.hoistFnE_ $ Avatar.parse >>> note InvalidAvatar

-- Utilities

cond :: forall a. (a -> Boolean) -> FormError -> a -> Either FormError a
cond f err a = if f a then pure a else Left err 

toOptional 
  :: ∀ form m a b
   . Monoid a 
  => Eq a
  => Monad m 
  => F.Validation form m FormError a b
  -> F.Validation form m FormError a (Maybe b)
toOptional v = F.Validation \form val -> 
  case val == mempty of
    true -> pure (pure Nothing)
    _ -> (map <<< map) Just (F.runValidation v form val)
