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
import Conduit.Api.Endpoint (Endpoint(..), endpointCodec)
import Conduit.Data.Email (Email)
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
-- | details.When actually using the API, we generally worry about which endpoint we'd like to 
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
-- | been included here because they operate on tokens. Remember: we can't create or manipulate 
-- | the `Token` type outside this module, so we'll somewhat awkwardly define the registration
-- | and login requests in this module. These requests will be the only way to create an auth
-- | token in the system.

-- | First, we'll define a few types as the inputs for our login and authentication requests.
-- |
-- | Both functions -- as well as functions from the `Conduit.Capability.Resource.User` module -- 
-- | share most of their required input fields. Rather than define three separate record types
-- | (which could fall out of sync with one another and introduce unnecessary boilerplate), we'll
-- | use PureScript's lovely extensible row types to share as much as possible among the inputs.
-- |
-- | This is possible because rows are a type-level list of keys and values, and these lists
-- | can be merged. Here's an example:
-- |
-- | ```purescript
-- | -- A row containing a label, "a" with a value of type `Int`, which can be extended with 
-- | -- another row
-- | type RowA r = (a :: Int | r) 
-- | 
-- | -- A row containing all labels from `RowA`, plus an additional label "b" with a value of 
-- | type `Int`, which can be extended by yet another row. The two versions below are equivalent.
-- | type RowAB r = (b :: Int | RowA r)
-- | type RowAB r = (a :: Int, b :: Int | r)
-- |
-- | -- A "closed" row, which simply means it cannot be extended with further rows.
-- | type RowABC = (c :: Int | RowA RowB)
-- |
-- | -- Records are really just rows under the hood, with nicer syntax
-- | type RecordABC = Record RowABC
-- | type RecordABC = { | RowABC }
-- | type RecordABC = { a :: Int, b :: Int, c :: Int }
-- | ```

-- | We're almost ready to get started, but we have a snag! We're going to define two record types 
-- | using most of the same underlying row. But in some records, we'll have a password field with 
-- | the type `Maybe String`, but in others, we'll have a password field with the type `String`. 
-- | How can we still share these rows?
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
-- |
-- | The types below are equivalent to:
-- |
-- | ```purescript
-- | type RegisterFields = { email :: Email, password :: String, username :: Username }
-- | type LoginFields = { email :: Email, password :: String }
-- | ```
-- | 
-- | Admittedly, it's not strictly necessary to share fields among the three auth types because
-- | they're so small. But this pattern is common in production applications and in libraries like
-- | `purescript-record`, `purescript-variant`, and `purescript-halogen-formless`, so it's
-- | important to be aware of it.
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
