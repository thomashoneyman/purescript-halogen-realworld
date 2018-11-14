module Data.AuthUser
  ( AuthUser -- constructors not exported
  ) where

import Prelude

import Data.Username (Username)

-- An `AuthUser` represents the currently-authenticated user. We will use this 
-- type throughout the application for auth purposes. We at least need their 
-- unique identifier (Username) and a JWT token.

data AuthUser = AuthUser Username Token

derive instance eqAuthUser :: Eq AuthUser

-- We don't want to show the token.

instance showAuthUser :: Show AuthUser where
  show (AuthUser u t) = "AuthUser " <> show u <> "{- token -}"

-- We are using a smart constructor to ensure the token can never be retrieved 
-- except with functions from this module. But that means the username will be 
-- hidden, too. For that reason we'll write a helper to retrieve it.

username :: AuthUser -> Username
username (AuthUser u _) = u

-- We don't export this type from the module and don't need any instances, so I 
-- feel comfortable with a type alias instead of a newtype.

type Token = String
