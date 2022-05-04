-- | This module exports various utilities for working with a REST API and Json. It also provides
-- | a few helpers shared among requests which I found useful when implementing the production
-- | monad, `Conduit.AppM`.
module Conduit.Api.Utils where

import Prelude

import Affjax.Web (request)
import Conduit.Api.Request (BaseURL, RequestOptions, Token, defaultRequest, readToken, writeToken)
import Conduit.Capability.LogMessages (class LogMessages, logError)
import Conduit.Capability.Now (class Now)
import Conduit.Data.Profile (Profile)
import Conduit.Data.Username (Username)
import Conduit.Store (Action(..), Store)
import Data.Argonaut.Core (Json)
import Data.Bifunctor (rmap)
import Data.Codec.Argonaut (JsonCodec, printJsonDecodeError)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen.Store.Monad (class MonadStore, getStore, updateStore)

-- | This function performs a request that does not require authentication by pulling the base URL
-- | out of the app environment and running an asynchronous request. This function only requires the
-- | `baseUrl` field from the app environment. See `Conduit.AppM` for examples of this in action.
mkRequest
  :: forall m
   . MonadAff m
  => MonadStore Action Store m
  => RequestOptions
  -> m (Maybe Json)
mkRequest opts = do
  { baseUrl } <- getStore
  response <- liftAff $ request $ defaultRequest baseUrl Nothing opts
  pure $ hush $ rmap _.body response

-- | This function performs a request that requires authentication by pulling the base URL out
-- | of the app environment, reading the auth token from local storage, and then performing
-- | the asynchronous request. See `Conduit.AppM` for examples of this in action.
mkAuthRequest
  :: forall m
   . MonadAff m
  => MonadStore Action Store m
  => RequestOptions
  -> m (Maybe Json)
mkAuthRequest opts = do
  { baseUrl } <- getStore
  token <- liftEffect readToken
  response <- liftAff $ request $ defaultRequest baseUrl token opts
  pure $ hush $ map _.body response

-- | Logging in and registering share a lot of behavior, namely updating the application environment
-- | and writing the auth token to local storage. This helper function makes it easy to layer those
-- | behaviors on top of the request. This also performs the work of broadcasting changes in the
-- | current user to all subscribed components.
authenticate
  :: forall m a
   . MonadAff m
  => MonadStore Action Store m
  => LogMessages m
  => Now m
  => (BaseURL -> a -> m (Either String (Tuple Token Profile)))
  -> a
  -> m (Maybe Profile)
authenticate req fields = do
  { baseUrl } <- getStore
  req baseUrl fields >>= case _ of
    Left err -> logError err *> pure Nothing
    Right (Tuple token profile) -> do
      liftEffect do
        writeToken token
      updateStore $ LoginUser profile
      pure (Just profile)

-- | This small utility decodes JSON and logs any failures that occurred, returning the parsed
-- | value only if decoding succeeded. This utility makes it easy to abstract the mechanices of
-- | dealing with malformed responses. See `Conduit.AppM` for examples of this in practice.
decode :: forall m a. LogMessages m => Now m => JsonCodec a -> Maybe Json -> m (Maybe a)
decode _ Nothing = logError "Response malformed" *> pure Nothing
decode codec (Just json) = case CA.decode codec json of
  Left err -> logError (printJsonDecodeError err) *> pure Nothing
  Right response -> pure (Just response)

-- | This small utility is similar to the prior `decode` function, but it's designed to work with
-- | decoders that require knowing the currently-authenticated user to work. For example, our
-- | `Profile` type depends on the currently-logged-in user (if there is one) to determine whether
-- | you are the author, you follow the author, or you don't follow the author. This utility
-- | handles the mechanics of retrieving the current user and providing the username to the
-- | provided decoder.
decodeWithUser
  :: forall m a
   . MonadEffect m
  => MonadStore Action Store m
  => LogMessages m
  => Now m
  => (Maybe Username -> JsonCodec a)
  -> Maybe Json
  -> m (Maybe a)
decodeWithUser codec json = do
  { currentUser } <- getStore
  decode (codec (_.username <$> currentUser)) json
