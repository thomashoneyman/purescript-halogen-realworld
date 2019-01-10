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
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Routing.Duplex (print)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, removeItem, setItem)

-- | As usual, we'll start with data types we'll leverage throughout the rest of this module. Let's
-- | start with a token, which will be used to authenticate requests to our server. This type is 
-- | necessary for requests, but we don't want to be able to inadvertently construct an invalid
-- | token, read its contents, or otherwise manipulate the string. We can achieve this using the
-- | smart constructor pattern: we'll create a new type called `Token` and we won't export its 
-- | constructor. The only functions that will be able to manipulate this value exist in this 
-- | module.
-- | 
-- | The only way to actually get one of these tokens is to use a request from this module (like 
-- | `login` or `register`). For that reason, any time we see this type used outside this module,
-- | we're guaranteed that it was retrieved properly before use.
newtype Token = Token String

-- | Notably, we'll skip the usual `newtype` instance so that we can't sneakily use the `wrap` or 
-- | `unwrap` functions to create a `Token`.
derive instance eqToken :: Eq Token
derive instance ordToken :: Ord Token

-- | We won't derive a `Show` instance because we don't ever want to reveal the token. Instead, we'll
-- | provide a manual instance.
instance showToken :: Show Token where
  show (Token _) = "Token {- token -}"

-- | Next, we'll give a meaningful type to the string that represents the base or root url of the 
-- | application. In my opinion, we don't need to be strict about how this string is constructed,
-- | because we hard-code it once in `Main` and that's it.
newtype BaseURL = BaseURL String

-- | Next, we'll create a type to represent the different methods our API supports. An API request
-- | can be GET, POST, PUT, or DELETE. These methods are already captured by the `Data.HTTP.Method`
-- | module, but I've opted for a custom type because, in our API, only `Post` and `Put` can have
-- | optional JSON bodies. Let's keep our invalid states unrepresentable by preventing a `Get`
-- | request with a JSON body.
data RequestMethod 
  = Get
  | Post (Maybe Json)
  | Put (Maybe Json)
  | Delete

-- | The BaseURL and token are incidental details in making a request. They are necessary, but not
-- | relevant to our business logic. This type, however, captures the two most vital pieces of 
-- | information necessary to make a request: where and how?
-- |
-- | The `Endpoint` type is a type-safe representation of the possible endpoints in our API. And we 
-- | defined the `RequestMethod` above.
type RequestOptions =
  { endpoint :: Endpoint
  , method :: RequestMethod
  }

-- | Finally, we can use our BaseURL, token, and request options to create a data structure
-- | that the `Affjax` library understands and can use to make asynchronous requests. We won't
-- | use this function too often directly; it's instead used as a helper in all the request 
-- | functions defined below and in the `Conduit.Api.Utils` modules.
defaultRequest :: BaseURL -> Maybe Token -> RequestOptions -> Request Json
defaultRequest (BaseURL baseUrl) auth { endpoint, method } =
  { method: Left method 
  , url: baseUrl <> print endpointCodec endpoint
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

-- | The following data types and functions aren't a natural fit for this module, but they've 
-- | been included here because they operate on tokens. We don't want to allow any reading or
-- | manipulation of auth tokens outside this module, so we'll somewhat awkwardly define 
-- | registration and login requests in this module and export them.

-- | First, we'll define a few data types as the inputs for our login and authentication requests.
-- |
-- | We're going to use the same underlying rows to construct multiple different kinds of records.
-- | This is possible because records are a type-level list of keys and values, and these lists
-- | can be merged. However, we have a snag: in some records, we'll have a password field with the 
-- | type `Maybe String`, but in others, we'll have a password field with the type `String`. 
-- | 
-- | We can reconcile this by defining our row so that the password field is in a "box" represented 
-- | by a type variable, `box`. This box can be filled in with various types of the kind 
-- | `Type -> Type`, like `Maybe a`, `Array a`, `List a`, and so on. For example:
-- |
-- | ```purescript
-- | type MyRecord box = { myValue :: box String }
-- | 
-- | myMaybeRecord :: MyRecord Maybe
-- | myMaybeRecord = { myValue: Just "" }
-- |
-- | myArrayRecord :: MyRecord Array
-- | myArrayRecord = { myValue: [ "", "", "" ] }
-- | ```
-- |
-- | We still need to be able to produce this record, however:
-- | 
-- | ```purescript
-- | myRecord :: MyRecord ???
-- | myRecord = { myValue: "" }
-- | ```
-- |
-- | How can we accomplish this? We've asserted that the `String` exists within some container,
-- | `box`, but it looks like we now need the `String` type NOT in a containing type.
-- |
-- | We can reconcile this with the `Unlifted` type synonym defined below. It's similar to the
-- | `identity` function in that it just hands back the type you provide. It's like a box that
-- | erases itself at compile-time. The correct version of `myRecord` is this:
-- |
-- | ```purescript
-- | myRecord :: MyRecord Unlifted
-- | myRecord = { myValue: "" }
-- | ```
type Unlifted a = a 

-- | Now we can define a shared row for various requests which manage user credentials. And the 
-- | password field can be a `Maybe String` or a `String`, depending on what we need!
-- |
-- | By convention, I give row types that will later be used as records the `-Rep` suffix.
type AuthFieldsRep box r = ( email :: Email, password :: box String | r )

-- | Our login and registration records both require a password field with a `String` value. 
-- | However, in the `Conduit.Capability.Resource.User` module, we'll see another record using  
-- | the same fields in which the password is a `Maybe String`. I encourage you to check it out
-- | to see this pattern in practice!
type RegisterFields = { | AuthFieldsRep Unlifted (username :: Username) }
type LoginFields = { | AuthFieldsRep Unlifted () }

-- | This function logs a user in (if they exist), returning an auth token and the user's 
-- | minimal profile.
login :: forall m. MonadAff m => BaseURL -> LoginFields -> m (Either String (Tuple Token Profile))
login baseUrl fields = 
  let method = Post $ Just $ encodeJson { user: fields } 
   in requestUser baseUrl { endpoint: Login, method }

-- | This function registers a user (if they don't already exist), returning an auth token and the
-- | user's minimal profile.
register :: forall m. MonadAff m => BaseURL -> RegisterFields -> m (Either String (Tuple Token Profile))
register baseUrl fields = 
  let method = Post $ Just $ encodeJson { user: fields }
   in requestUser baseUrl { endpoint: Users, method } 

-- | The login and registration requests share the same underlying implementation, just a different 
-- | endpoint. This function can be re-used by both requests.
requestUser :: forall m. MonadAff m => BaseURL -> RequestOptions -> m (Either String (Tuple Token Profile))
requestUser baseUrl opts = do
  res <- liftAff $ request $ defaultRequest baseUrl Nothing opts
  pure $ decodeAuthProfile =<< (_ .: "user") =<< decodeJson =<< lmap printResponseFormatError res.body

-- | This JSON decoder is defined in this module because it manipulates a token. First, we'll decode
-- | only the token field from the payload, and then we'll decode everything else separately into 
-- | the user's profile.
decodeAuthProfile :: Json -> Either String (Tuple Token Profile)
decodeAuthProfile json = do
  str <- decodeJson =<< (_ .: "token") =<< decodeJson json
  prof <- decodeJson json
  pure $ Tuple (Token str) prof

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
