module Conduit.Api.Utils where

import Prelude

import Affjax (request)
import Conduit.Api.Request (BaseURL, RequestOptions, defaultRequest, readToken)
import Control.Monad.Reader (class MonadAsk, ask)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Either (Either, hush)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)

-- Decode JSON within a particular key. For example: decode a Profile object
-- within a larger object containing a "user" field with:
--
-- decodeProfile :: Json -> Either String Profile
-- decodeProfile = decodeAt "user"
decodeAt :: forall a. DecodeJson a => String -> Json -> Either String a
decodeAt key = decodeJson <=< (_ .: key) <=< decodeJson

mkRequest 
  :: forall m r
   . MonadAff m
  => MonadAsk { baseUrl :: BaseURL | r } m
  => RequestOptions
  -> m (Maybe Json)
mkRequest opts = do
  { baseUrl } <- ask
  res <- liftAff $ request $ defaultRequest baseUrl Nothing opts
  pure $ hush res.body

mkAuthRequest 
  :: forall m r
   . MonadAff m
  => MonadAsk { baseUrl :: BaseURL | r } m
  => RequestOptions
  -> m (Maybe Json)
mkAuthRequest opts = do
  { baseUrl } <- ask
  token <- liftEffect readToken
  res <- liftAff $ request $ defaultRequest baseUrl token opts
  pure $ hush res.body
