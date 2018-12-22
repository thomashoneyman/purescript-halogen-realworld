module Conduit.Api.Request 
  ( Token -- constructor and decoders not exported
  , BaseURL(..)
  , RequestMethod(..)
  , RequestOptions(..)
  , defaultRequest
  , Unlifted(..)
  , RegisterFields(..)
  , LoginFields(..)
  , AuthFieldsRep(..)
  , login
  , register
  , readToken
  , writeToken
  , removeToken
  ) where

import Prelude

import Affjax (Request, printResponseFormatError, request)
import Affjax.RequestBody as RB
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as RF
import Conduit.Data.Email (Email)
import Conduit.Data.Endpoint (Endpoint(..), endpointCodec)
import Conduit.Data.Profile (Profile)
import Conduit.Data.Username (Username)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson, (.:))
import Data.Argonaut.Encode (encodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Routing.Duplex (print)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, removeItem, setItem)

-- Constructor hidden so that tokens can't be inadvertently read or arbitrarily created. 
-- One downside of this is that any function that manipulates a token must be defined in 
-- this module and exported, but the upside is that we always know the token was retrieved
-- properly before use.
newtype Token = Token String

derive instance eqToken :: Eq Token
derive instance ordToken :: Ord Token

-- We don't want to inadvertently show the user's token, so we'll write a manual
-- `Show` instance

instance showToken :: Show Token where
  show (Token _) = "Token {- token -}"

-- We build a BaseURL newtype to have a meaningful type to pass around in parameters
-- a more powerful type that does validation could be an improvement.
newtype BaseURL = BaseURL String

derive instance newtypeBaseURL :: Newtype BaseURL _

data RequestMethod 
  = Get
  | Post (Maybe Json)
  | Put (Maybe Json)
  | Delete

type RequestOptions =
  { endpoint :: Endpoint
  , method :: RequestMethod
  }

defaultRequest :: BaseURL -> Maybe Token -> RequestOptions -> Request Json
defaultRequest baseUrl auth { endpoint, method } =
  { method: Left method 
  , url: unwrap baseUrl <> print endpointCodec endpoint
  , headers: case auth of
      Nothing -> []
      Just (Token t) -> [ RequestHeader "Authorization" $ "Token " <> t ]
  , content: RB.json <$> body
  , username: Nothing
  , password: Nothing
  , withCredentials: false
  , responseFormat: RF.json
  }
  where
  Tuple method body = case method of
    Get -> Tuple GET Nothing
    Post b -> Tuple POST b
    Put b -> Tuple PUT b
    Delete -> Tuple DELETE Nothing

-- The `Unlifted` type here allows us to turn any concrete type of kind  `Type` 
-- into a type with a single argument of kind `(Type -> Type)`. This is useful
-- when we want to represent a value within various 'boxes'. For example, our
-- we might like our `AuthFieldsRep` type to cover the possibilites that a password
-- has a `Maybe` value `(Maybe String)`, an array value `(Array String)`, or a 
-- simple `String` value `(Unlifted String)`.
type Unlifted a = a 

-- For example, in both of these cases, we want to ensure a password was provided.
-- However, in the Capability.Resource.User module we'll see an example of when this 
-- should be a `Maybe` value instead.
type RegisterFields = { | AuthFieldsRep Unlifted (username :: Username) }
type LoginFields = { | AuthFieldsRep Unlifted () }

-- The underlying shared row for various requests which manage user credentials.
type AuthFieldsRep f r = ( email :: Email, password :: f String | r )

-- For decoding a user response from the server into an Token + Profile. Not exported
-- in order to ensure that the only way to acquire a token is via our user management 
-- functions.
decodeAuthProfile :: Json -> Either String (Tuple Token Profile)
decodeAuthProfile json = do
  str <- decodeJson =<< (_ .: "token") =<< decodeJson json
  prof <- decodeJson json
  pure $ Tuple (Token str) prof

-- Required to write in this module because it operates on tokens
login :: forall m. MonadAff m => BaseURL -> LoginFields -> m (Either String (Tuple Token Profile))
login baseUrl fields = 
  let method = Post $ Just $ encodeJson { user: fields } 
   in requestUser baseUrl { endpoint: Login, method }

-- Required to write in this module because it operates on tokens
register :: forall m. MonadAff m => BaseURL -> RegisterFields -> m (Either String (Tuple Token Profile))
register baseUrl fields = 
  let method = Post $ Just $ encodeJson { user: fields }
   in requestUser baseUrl { endpoint: Users, method } 

-- Same underlying mechanism for both requests
requestUser :: forall m. MonadAff m => BaseURL -> RequestOptions -> m (Either String (Tuple Token Profile))
requestUser baseUrl opts = do
  res <- liftAff $ request $ defaultRequest baseUrl Nothing opts
  pure $ decodeAuthProfile =<< (_ .: "user") =<< decodeJson =<< lmap printResponseFormatError res.body

-- Managing tokens in local storage

tokenKey = "token" :: String

readToken :: Effect (Maybe Token)
readToken = do
  str <- getItem tokenKey =<< localStorage =<< window
  pure $ map Token str

writeToken :: Token -> Effect Unit
writeToken (Token str) =
  setItem tokenKey str =<< localStorage =<< window

removeToken :: Effect Unit
removeToken =
  removeItem tokenKey =<< localStorage =<< window
