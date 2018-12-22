module Conduit.Api.Request where

import Prelude

import Affjax (Request)
import Affjax.RequestBody as RB
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as RF
import Conduit.Data.Endpoint (Endpoint, endpointCodec)
import Data.Argonaut.Core (Json)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Routing.Duplex (print)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, removeItem, setItem)

-- Constructor hidden so that tokens can't be inadvertently read
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

data RequestType 
  = Get
  | Post (Maybe Json)
  | Put (Maybe Json)
  | Delete

type RequestOptions =
  { endpoint :: Endpoint
  , requestType :: RequestType
  }

data ApiResponse 
  = Error Json
  | Success Json

defaultRequest :: BaseURL -> Maybe Token -> RequestOptions -> Request Json
defaultRequest baseUrl auth { endpoint, requestType } =
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
  Tuple method body = case requestType of
    Get -> Tuple GET Nothing
    Post b -> Tuple POST b
    Put b -> Tuple PUT b
    Delete -> Tuple DELETE Nothing

-- -- For auth tokens specifically, because the decoder is not exposed

-- login :: forall m. MonadAff m => LoginFields -> BaseURL -> m (Either String (Tuple Token Profile))
-- login body = 
--   runRequest (decodeAuthProfile <=< (_ .: "user") <=< decodeJson) 
--     <<< post NoAuth (Just $ encodeJson { user: body }) Login

-- register :: forall m. MonadAff m => RegisterFields -> BaseURL -> m (Either String (Tuple Token Profile))
-- register body = 
--   runRequest (decodeAuthProfile <=< (_ .: "user") <=< decodeJson)
--     <<< post NoAuth (Just $ encodeJson { user: body }) Users

-- -- For decoding a user response from the server into an Token + Profile

-- decodeAuthProfile :: Json -> Either String (Tuple Token Profile)
-- decodeAuthProfile json = do
--   str <- (_ .: "token") =<< decodeJson json
--   prof <- decodeJson json
--   pure $ Tuple (Token str) prof

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
