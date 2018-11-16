-- | This module exposes data types and functions related to authenticated requests
-- | in the system. It hides information about the current user so sensitive info
-- | like the user's auth token are not exposed and authenticated users can only
-- | be created via functions exposed here. They can be re-used as the implementation
-- | of our authentication capability for AppM.

module Data.Request
  ( AuthUser -- constructors not exported
  , username
  , authUserKey
  ) where

import Prelude

import Data.Argonaut (Json, decodeJson, jsonEmptyObject, (.?), (:=), (~>))
import Data.Either (Either)
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

-- We need to be able to retrieve an AuthUser. This won't perform any effects to 
-- do so, but it'll provide the necessary parsing.

-- Do not export.
decodeAuthUser :: Json -> Either String AuthUser
decodeAuthUser json = do
  obj <- decodeJson json
  uname <- obj .? "username"
  tok <- obj .? "token"
  pure $ AuthUser uname tok

-- Do not export.
encodeAuthUser :: AuthUser -> Json
encodeAuthUser (AuthUser uname tok) =
  "username" := uname
    ~> "token" := tok
    ~> jsonEmptyObject

authUserKey = "authUser" :: String