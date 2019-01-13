-- | This module exports various utilities for working with a REST API and Json. It also provides
-- | a few helpers shared among requests which I found useful when implementing the production 
-- | monad, `Conduit.AppM`.
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

-- | This function performs a request that does not require authentication by pulling the base URL 
-- | out of the app environment and running an asynchronous request. This function only requires the 
-- | `baseUrl` field from the app environment. See `Conduit.AppM` for examples of this in action.
mkRequest 
  :: forall m r
   . MonadAff m
  => MonadAsk { baseUrl :: BaseURL | r } m
  => RequestOptions
  -> m (Maybe Json)
mkRequest opts = do
  { baseUrl } <- ask
  response <- liftAff $ request $ defaultRequest baseUrl Nothing opts
  pure $ hush response.body

-- | This function performs a request that requires authentication by pulling the base URL out
-- | of the app environment, reading the auth token from local storage, and then performing
-- | the asynchronous request. See `Conduit.AppM` for examples of this in action.
mkAuthRequest 
  :: forall m r
   . MonadAff m
  => MonadAsk { baseUrl :: BaseURL | r } m
  => RequestOptions
  -> m (Maybe Json)
mkAuthRequest opts = do
  { baseUrl } <- ask
  token <- liftEffect readToken
  response <- liftAff $ request $ defaultRequest baseUrl token opts
  pure $ hush response.body

-- | Logging in and registering share a lot of behavior, namely updating the application environment
-- | and writing the auth token to local storage. This helper function makes it easy to layer those
-- | behaviors on top of the request.
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

-- | We can decode records and primitive types automatically, and we've defined custom decoders for
-- | our custom data types. However, our API frequently returns those data structures wrapped in 
-- | a larger object with a single field like "user", "profile", or "article". This utility allows
-- | us to decode a JSON object with a particular key, and then decode the contents. 
-- |
-- | For example, consider this JSON object containing a single field, "user", which itself contains 
-- | a JSON object representing a user profile:
-- |
-- | ```json
-- | { "user": { "username": ... } }
-- | ```
-- | 
-- | We can make our `Profile` decoder compatible with this new JSON using our `decodeAt` helper:
-- |
-- | ```purescript
-- | decodeProfile :: Json -> Either String Profile
-- | decodeProfile = decodeAt "user"
-- | ```
decodeAt :: forall a. DecodeJson a => String -> Json -> Either String a
decodeAt key = decodeJson <=< (_ .: key) <=< decodeJson

-- | This small utility decodes JSON and logs any failures that occurred, returning the parsed 
-- | value only if decoding succeeded. This utility makes it easy to abstract the mechanices of 
-- | dealing with malformed responses. See `Conduit.AppM` for examples of this in practice.
decode :: forall m a. LogMessages m => Now m => (Json -> Either String a) -> Maybe Json -> m (Maybe a)
decode _ Nothing = logError "Response malformed" *> pure Nothing 
decode decoder (Just json) = case decoder json of
  Left err -> logError err *> pure Nothing
  Right response -> pure (Just response)

-- | This small utility is similar to the prior `decode` function, but it's designed to work with
-- | decoders that require knowing the currently-authenticated user to work. For example, our 
-- | `Profile` type depends on the currently-logged-in user (if there is one) to determine whether
-- | you are the author, you follow the author, or you don't follow the author. This utility 
-- | handles the mechanics of retrieving the current user and providing the username to the 
-- | provided decoder.
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
  maybeProfile <- (liftEffect <<< Ref.read) =<< asks _.currentUser
  decode (decoder (_.username <$> maybeProfile)) json
