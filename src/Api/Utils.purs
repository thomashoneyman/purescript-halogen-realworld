module Conduit.Api.Utils where

import Prelude

import Affjax (request)
import Conduit.Api.Request (BaseURL, RequestOptions, Token, defaultRequest, readToken, writeToken)
import Conduit.Capability.LogMessages (class LogMessages, logError)
import Conduit.Capability.Now (class Now)
import Conduit.Data.Profile (Profile)
import Conduit.Data.Username (Username)
import Control.Monad.Reader (class MonadAsk, ask, asks)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref

-- Decode JSON within a particular key. For example: decode a Profile object
-- within a larger object containing a "user" field with:
--
-- decodeProfile :: Json -> Either String Profile
-- decodeProfile = decodeAt "user"
decodeAt :: forall a. DecodeJson a => String -> Json -> Either String a
decodeAt key = decodeJson <=< (_ .: key) <=< decodeJson

-- Perform a request that does not require authentication
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

-- Perform a request that requires authentication
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

-- Shared behaviors for logging in or registering
authenticate 
  :: forall m a r
   . MonadAff m
  => MonadAsk { baseUrl :: BaseURL, currentUser :: Ref (Maybe Profile) | r } m
  => LogMessages m
  => Now m
  => (BaseURL -> a -> m (Either String (Tuple Token Profile))) 
  -> a 
  -> m (Maybe Profile)
authenticate req fields = do 
  { baseUrl, currentUser } <- ask
  req baseUrl fields >>= case _ of
    Left err -> logError err *> pure Nothing
    Right (Tuple token profile) -> do 
      liftEffect $ writeToken token 
      liftEffect $ Ref.write (Just profile) currentUser
      pure (Just profile)

-- A small utility to log out decoding failures
decode :: forall m a. LogMessages m => Now m => (Json -> Either String a) -> Maybe Json -> m (Maybe a)
decode _ Nothing = logError "Response malformed" *> pure Nothing 
decode decoder (Just json) = case decoder json of
  Left err -> logError err *> pure Nothing
  Right res -> pure (Just res)

-- A small utility to help with decoders that require the current user as an argument
decodeWithUser 
  :: forall m a r
   . MonadEffect m
  => MonadAsk { currentUser :: Ref (Maybe Profile) | r } m
  => LogMessages m 
  => Now m
  => (Maybe Username -> Json -> Either String a) 
  -> Maybe Json 
  -> m (Maybe a)
decodeWithUser decoder json = do
  mbProfile <- (liftEffect <<< Ref.read) =<< asks _.currentUser
  decode (decoder (_.username <$> mbProfile)) json
