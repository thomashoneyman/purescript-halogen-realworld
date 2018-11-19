module Api.Request
  ( AuthToken -- constructors not exported
  , authTokenKey
  , readAuthTokenFromLocalStorage
  , writeAuthTokenToLocalStorage
  , deleteAuthTokenFromLocalStorage
  , get
  , post
  , put
  , delete
  , AuthType(..)
  , runRequest
  , runRequest'
  , runRequestAuth
  ) where

import Prelude

import Affjax as AX
import Affjax.RequestBody as RB
import Affjax.RequestHeader as RH
import Affjax.ResponseFormat as RF
import Api.Endpoint (Endpoint, endpointCodec)
import Data.Argonaut (class DecodeJson, Json, decodeJson, encodeJson, jsonParser, stringify)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), note)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Routing.Duplex (print)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, removeItem, setItem)

-- We need a token in order to authenticate requests in the application. However,
-- we also want to ensure that this value never gets constructed except via logging
-- in, registering, or reading from local storage. This data type will be used
-- in this module for helper functions to construct request.

newtype AuthToken = AuthToken String

derive instance eqAuthToken :: Eq AuthToken
derive instance ordAuthToken :: Ord AuthToken

-- We don't want to inadvertently show the user's token, so we'll write a manual
-- `Show` instance

instance showAuthToken :: Show AuthToken where
  show (AuthToken _) = "AuthToken {- token -}"

-- We need to be able to serialize and de-serialize AuthTokens from JSON. We won't
-- export these functions, but they'll provide necessary parsing for our request
-- functions.

-- Do not export.
decodeAuthToken :: Json -> Either String AuthToken
decodeAuthToken = (pure <<< AuthToken) <=< decodeJson

-- Do not export.
encodeAuthToken :: AuthToken -> Json
encodeAuthToken (AuthToken t) = encodeJson t

authTokenKey = "authToken" :: String

-- These functions can be exported; they construct or use an auth user and its
-- information but don't allow the end user to see anything internal, like the
-- specific token of a given user. Because these are used to implement our
-- Authenticate capability, our test monad can use different implementations like
-- dummy data instead.

readAuthTokenFromLocalStorage :: Effect (Either String AuthToken)
readAuthTokenFromLocalStorage = do
  tok <- getItem authTokenKey =<< localStorage =<< window
  pure $ decodeAuthToken =<< jsonParser =<< note "Failed to retrieve token" tok

writeAuthTokenToLocalStorage :: AuthToken -> Effect Unit
writeAuthTokenToLocalStorage au =
  setItem authTokenKey (stringify $ encodeAuthToken au) =<< localStorage =<< window

deleteAuthTokenFromLocalStorage :: Effect Unit
deleteAuthTokenFromLocalStorage =
  removeItem authTokenKey =<< localStorage =<< window

-- We'll represent authenticated and non-authenticated requests with a custom type
-- to avoid ambiguity over what a `Nothing` value means.
data AuthType = NoAuth | Auth AuthToken

derive instance eqAuthType :: Eq AuthType
derive instance ordAuthType :: Ord AuthType

-- We need to be able to make requests. Here, we'll assume that we've been provided
-- with an `AuthToken`. In practice, the only way to get an `AuthToken` is via our
-- `Authenticate` capability, so we'll leverage that to actually perform requests.
-- These data types create the request data type, but do not actually perform effects.

get :: AuthType -> Endpoint -> AX.Request Json
get auth = mkRequest GET auth Nothing

post :: AuthType -> Json -> Endpoint -> AX.Request Json
post auth body = mkRequest POST auth (Just body)

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
      Auth (AuthToken t) -> [ RH.RequestHeader "Authorization" $ "Token " <> t ]
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

-- With the type class

runRequest' :: forall m a. MonadAff m => DecodeJson a => AX.Request Json -> m (Either String a)
runRequest' = runRequest decodeJson

-- For auth tokens specifically, because the decoder is not exposed

runRequestAuth :: forall m. MonadAff m => AX.Request Json -> m (Either String AuthToken)
runRequestAuth = runRequest decodeAuthToken