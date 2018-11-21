module Api.Request
  ( AuthUser -- constructors not exported
  , username
  , authUserKey
  , readAuthUserFromLocalStorage
  , writeAuthUserToLocalStorage
  , deleteAuthUserFromLocalStorage
  , get
  , post
  , put
  , delete
  , AuthType(..)
  , runRequest
  , RegisterFields
  , LoginFields
  , login
  , register
  ) where

import Prelude

import Affjax as AX
import Affjax.RequestBody as RB
import Affjax.RequestHeader as RH
import Affjax.ResponseFormat as RF
import Api.Endpoint (Endpoint(..), endpointCodec)
import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Decode (decodeJson, (.?))
import Data.Argonaut.Encode (encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), note)
import Data.Email (Email)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Profile (Profile)
import Data.Tuple (Tuple(..))
import Data.Username (Username)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Routing.Duplex (print)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, removeItem, setItem)

-- We need a token in order to authenticate requests in the application. However,
-- we also want to ensure that this value never gets constructed except via logging
-- in, registering, or reading from local storage. We also need to be able to 
-- retrieve the username of the currently logged-in user at any point. We could
-- store that in global state, but for the sake of keeping related data together
-- and not having to invalidate / update in two places, we'll keep it in storage.

data AuthUser = AuthUser Username String

derive instance eqAuthUser :: Eq AuthUser
derive instance ordAuthUser :: Ord AuthUser

-- We don't want to inadvertently show the user's token, so we'll write a manual
-- `Show` instance

instance showAuthUser :: Show AuthUser where
  show (AuthUser uname _) = "AuthUser (" <> show uname <> ") {- token -}"

-- We've need to be able to retrieve the username, but not the token

username :: AuthUser -> Username
username (AuthUser uname _) = uname

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
encodeAuthUser (AuthUser uname t) = encodeJson { username: uname, token: t }

authUserKey = "authUser" :: String

-- These functions can be exported; they construct or use an auth user and its
-- information but don't allow the end user to see anything internal, like the
-- specific token of a given user. Because these are used to implement our
-- Authenticate capability, our test monad can use different implementations like
-- dummy data instead.

readAuthUserFromLocalStorage :: Effect (Either String AuthUser)
readAuthUserFromLocalStorage = do
  rec <- getItem authUserKey =<< localStorage =<< window
  pure $ decodeAuthUser =<< jsonParser =<< note "Failed to retrieve token" rec

writeAuthUserToLocalStorage :: AuthUser -> Effect Unit
writeAuthUserToLocalStorage au =
  setItem authUserKey (stringify $ encodeAuthUser au) =<< localStorage =<< window

deleteAuthUserFromLocalStorage :: Effect Unit
deleteAuthUserFromLocalStorage =
  removeItem authUserKey =<< localStorage =<< window

-- We'll represent authenticated and non-authenticated requests with a custom type
-- to avoid ambiguity over what a `Nothing` value means.
data AuthType = NoAuth | Auth AuthUser

derive instance eqAuthType :: Eq AuthType
derive instance ordAuthType :: Ord AuthType

-- We need to be able to make requests. Here, we'll assume that we've been provided
-- with an `AuthUser`. In practice, the only way to get an `AuthUser` is via our
-- `Authenticate` capability, so we'll leverage that to actually perform requests.
-- These data types create the request data type, but do not actually perform effects.

get :: AuthType -> Endpoint -> AX.Request Json
get auth = mkRequest GET auth Nothing

post :: AuthType -> Maybe Json -> Endpoint -> AX.Request Json
post auth = mkRequest POST auth

put :: AuthType -> Json -> Endpoint -> AX.Request Json
put auth body = mkRequest PUT auth (Just body)

delete :: AuthType -> Endpoint -> AX.Request Json
delete auth = mkRequest DELETE auth Nothing

-- The underlying helper to construct a request 

mkRequest :: Method -> AuthType -> Maybe Json -> Endpoint -> AX.Request Json
mkRequest method auth body endpoint =
  { method: Left method 
  , url: print endpointCodec endpoint 
  , headers: case auth of
      NoAuth -> []
      Auth (AuthUser _ t) -> [ RH.RequestHeader "Authorization" $ "Token " <> t ]
  , content: RB.json <$> body
  , username: Nothing
  , password: Nothing
  , withCredentials: false
  , responseFormat: RF.json
  }

-- Helper to actually run the request in Aff

runRequest :: forall m a. MonadAff m => (Json -> Either String a) -> AX.Request Json -> m (Either String a)
runRequest decode req = do
  res <- liftAff (AX.request req)
  pure (decode =<< lmap RF.printResponseFormatError res.body)


-- Fields necessary to authenticate a user. Could be done with extensible rows, but 
-- there aren't enough fields to justify it, IMO

type RegisterFields =
  { username :: Username
  , email :: Email
  , password :: String
  }

type LoginFields = 
  { email :: Email
  , password :: String 
  }

-- For auth tokens specifically, because the decoder is not exposed

login :: forall m. MonadAff m => LoginFields -> m (Either String (Tuple AuthUser Profile))
login body = runRequest decodeUser $ post NoAuth (Just $ encodeJson body) Login

register :: forall m. MonadAff m => RegisterFields -> m (Either String (Tuple AuthUser Profile))
register body = runRequest decodeUser $ post NoAuth (Just $ encodeJson body) Users

-- For decoding a user response from the server into an AuthUser + Profile

decodeUser :: Json -> Either String (Tuple AuthUser Profile)
decodeUser json = do
  au <- decodeAuthUser json
  prof <- decodeJson json
  pure $ Tuple au prof