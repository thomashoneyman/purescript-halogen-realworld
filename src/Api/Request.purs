-- | For now, our production app manages resources and fetches data using a REST API. This module
-- | defines several data types and helper functions to create and manage these requests. That
-- | includes managing auth tokens, designing types to represent possible requests, and more.
-- |
-- | While interesting, this module is largely mechanical. It helps provide most of the low-level
-- | functions that our production monad will leverage in `Conduit.AppM` to implement our various
-- | app capabilities.
module Conduit.Api.Request
  ( Token -- constructor and decoders not exported
  , BaseURL(..)
  , RequestMethod(..)
  , RequestOptions(..)
  , defaultRequest
  , RegisterFields(..)
  , LoginFields(..)
  , login
  , register
  , readToken
  , writeToken
  , removeToken
  ) where

import Prelude

import Affjax.RequestBody as RB
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as RF
import Affjax.Web (Request, printError, request)
import Conduit.Api.Endpoint (Endpoint(..), endpointCodec)
import Conduit.Data.Email (Email)
import Conduit.Data.Email as Email
import Conduit.Data.Profile (Profile)
import Conduit.Data.Profile as Profile
import Conduit.Data.Username (Username)
import Conduit.Data.Username as Username
import Data.Argonaut.Core (Json)
import Data.Bifunctor (lmap)
import Data.Codec as Codec
import Data.Codec.Argonaut (JsonCodec, JsonDecodeError, printJsonDecodeError)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Routing.Duplex (print)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, removeItem, setItem)

-- | As usual, we'll start with types we'll use throughout this module.

-- | Conduit uses a REST API and secures certain endpoints using a JSON Web Token (JWT). That means
-- | most requests will use the token to attach an "Authorization" header. It's critical that this
-- | token is not inadvertently exposed and that we attach a correct token to requests. We can
-- | prevent the token from being changed and restrict the ways it can be created using the smart
-- | constructor pattern. To learn more about this pattern, see:
-- |
-- | https://thomashoneyman.com/guides/real-world-halogen/design-data-pure-functions/#restricting-the-domain-using-smart-constructors
-- |
-- | To achieve this, we'll create a newtype without exporting its constructor. That means that
-- | the only functions that will be able to access the string within the `Token` type are in
-- | this module (like `login` and `register`). We can be confident that a `Token` used outside
-- | this module was retrieved properly and hasn't been tampered with in the meantime.
newtype Token = Token String

-- | No `newtype` instance allowed! That would allow us to use the `wrap` and `unwrap` functions to
-- | access the string within the `Token` constructor.
derive instance eqToken :: Eq Token

derive instance ordToken :: Ord Token

-- | We won't derive a `Show` instance, either, because we don't ever want to reveal the token.
-- | Instead, we'll provide a manual instance.
instance showToken :: Show Token where
  show (Token _) = "Token {- token -}"

-- | Next, we'll give a meaningful type to the string that represents the base or root url of the
-- | application. In my opinion, we don't need to be nearly so strict about how this string is
-- | constructed, because we hard-code it once in `Main` and never touch it again. We could
-- | reasonably have used the smart constructor pattern, though.
newtype BaseURL = BaseURL String

-- | Next, we'll create a type to represent the different methods our API supports. An API request
-- | can be GET, POST, PUT, or DELETE. These methods are already captured by the `Data.HTTP.Method`
-- | module, but I've opted for a custom type because, in our API, only `Post` and `Put` can have
-- | optional JSON bodies. There should never be a `Get` request with a JSON body. This type
-- | prevents that from occurring.
data RequestMethod
  = Get
  | Post (Maybe Json)
  | Put (Maybe Json)
  | Delete

-- | The base url and token are necessary to make a request, but are low-level implementation
-- | details. When actually using the API, we generally worry about which endpoint we'd like to
-- | access and what request method we'd like to use.
-- |
-- | The `RequestOptions` type below relies on the `Endpoint` data type (which captures the possible
-- | API locations we can access) and the `RequestMethod` type we just defined (which describes how
-- | we access those endpoints).
-- |
-- | As a brief aside: even that information is too low-level for our business logic! In the
-- | rest of the application we'll use functions that work on the resources we're managing without
-- | caring about the implementation (REST, RPC, local files, mocks, etc.). This type will be
-- | used to write a REST implementation for our resource management capabilities.
-- |
-- | Check out the `Conduit.Capability.Resource.*` modules for examples of the high-level functions
-- | we'll use in our business logic.
type RequestOptions =
  { endpoint :: Endpoint
  , method :: RequestMethod
  }

-- | Let's bring these `BaseURL`, `Token`, and `RequestOptions` types together to create a data
-- | structure that the `Affjax` library understands and can use to make asynchronous requests.
-- | We won't use this function too often directly; it's instead used as a helper in all the
-- | request functions defined below and in the `Conduit.Api.Utils` modules.
defaultRequest :: BaseURL -> Maybe Token -> RequestOptions -> Request Json
defaultRequest (BaseURL baseUrl) auth { endpoint, method } =
  { method: Left requestMethod
  , url: baseUrl <> print endpointCodec endpoint
  , headers: case auth of
      Nothing -> []
      Just (Token t) -> [ RequestHeader "Authorization" $ "Token " <> t ]
  , content: RB.json <$> body
  , username: Nothing
  , password: Nothing
  , timeout: Nothing
  , withCredentials: false
  , responseFormat: RF.json
  }
  where
  Tuple requestMethod body = case method of
    Get -> Tuple GET Nothing
    Post b -> Tuple POST b
    Put b -> Tuple PUT b
    Delete -> Tuple DELETE Nothing

-- | The following data types and functions aren't a natural fit for this module, but they've
-- | been included here because they operate on tokens. Remember: we can't create or manipulate
-- | the `Token` type outside this module, so we'll somewhat awkwardly define the registration
-- | and login requests in this module. These requests will be the only way to create an auth
-- | token in the system.
type RegisterFields =
  { email :: Email
  , password :: String
  , username :: Username
  }

registerCodec :: JsonCodec RegisterFields
registerCodec =
  CAR.object "RegisterFields"
    { email: Email.codec
    , password: CA.string
    , username: Username.codec
    }

type LoginFields =
  { email :: Email
  , password :: String
  }

loginCodec :: JsonCodec LoginFields
loginCodec =
  CAR.object "LoginFields"
    { email: Email.codec
    , password: CA.string
    }

-- | This function logs a user in (if they exist), returning an auth token and the user's
-- | minimal profile.
login :: forall m. MonadAff m => BaseURL -> LoginFields -> m (Either String (Tuple Token Profile))
login baseUrl fields =
  let
    method = Post $ Just $ Codec.encode (CAR.object "User" { user: loginCodec }) { user: fields }
  in
    requestUser baseUrl { endpoint: Login, method }

-- | This function registers a user (if they don't already exist), returning an auth token and the
-- | user's minimal profile.
register :: forall m. MonadAff m => BaseURL -> RegisterFields -> m (Either String (Tuple Token Profile))
register baseUrl fields =
  let
    method = Post $ Just $ Codec.encode (CAR.object "User" { user: registerCodec }) { user: fields }
  in
    requestUser baseUrl { endpoint: Users, method }

-- | The login and registration requests share the same underlying implementation, just a different
-- | endpoint. This function can be re-used by both requests.
requestUser :: forall m. MonadAff m => BaseURL -> RequestOptions -> m (Either String (Tuple Token Profile))
requestUser baseUrl opts = do
  res <- liftAff $ request $ defaultRequest baseUrl Nothing opts
  case res of
    Left e -> pure $ Left $ printError e
    Right v -> pure $ lmap printJsonDecodeError $ decodeAuthProfile =<< Codec.decode (CAR.object "User" { user: CA.json }) v.body

-- | This JSON decoder is defined in this module because it manipulates a token. First, we'll decode
-- | only the token field from the payload, and then we'll decode everything else separately into
-- | the user's profile.
decodeAuthProfile :: { user :: Json } -> Either JsonDecodeError (Tuple Token Profile)
decodeAuthProfile { user } = do
  { token } <- Codec.decode (CAR.object "Token" { token: tokenCodec }) user
  profile <- Codec.decode Profile.profileCodec user
  pure (Tuple token profile)
  where
  tokenCodec =
    CA.prismaticCodec "Token (inner)" (Just <<< Token) (\(Token t) -> t) CA.string

-- | The following functions deal with writing, reading, and deleting tokens in local storage at a
-- | particular key. They'll be used as part of our production monad, `Conduit.AppM`.

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
