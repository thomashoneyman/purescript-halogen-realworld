-- | This module exposes data types and functions related to authenticated requests
-- | in the system. It hides information about the current user so sensitive info
-- | like the user's auth token are not exposed and authenticated users can only
-- | be created via functions exposed here. They can be re-used as the implementation
-- | of our authentication capability for AppM.

module Data.AuthUser
  ( AuthUser -- constructors not exported
  , username
  , authUserKey
  , readAuthUserFromLocalStorage
  , writeAuthUserToLocalStorage
  , deleteAuthUserFromLocalStorage
  ) where

import Prelude

import Data.Argonaut (Json, decodeJson, jsonEmptyObject, jsonParser, stringify, (.?), (:=), (~>))
import Data.Either (Either, note)
import Data.Username (Username)
import Effect (Effect)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, removeItem, setItem)

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

-- We need to be able to serialize and de-serialize AuthUsers from JSON. We won't
-- export these functions, but they'll provide necessary parsing for our request
-- functions.

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

-- These functions can be exported; they construct or use an auth user and its
-- information but don't allow the end user to see anything internal, like the
-- specific token of a given user.

readAuthUserFromLocalStorage :: Effect (Either String AuthUser)
readAuthUserFromLocalStorage = do
  tok <- getItem authUserKey =<< localStorage =<< window
  pure $ decodeAuthUser =<< jsonParser =<< note "Failed to retrieve token" tok

writeAuthUserToLocalStorage :: AuthUser -> Effect Unit
writeAuthUserToLocalStorage au = do
  setItem authUserKey (stringify $ encodeAuthUser au)
    =<< localStorage
    =<< window

deleteAuthUserFromLocalStorage :: Effect Unit
deleteAuthUserFromLocalStorage = do
  removeItem authUserKey =<< localStorage =<< window