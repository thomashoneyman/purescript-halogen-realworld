module Conduit.Api.Utils
  ( withUser
  , withAuthUser
  , withAuthUser_
  ) where

import Prelude

import Affjax (Request)
import Conduit.Api.Request (AuthUser, BaseURL, runRequest, username)
import Conduit.Capability.Authenticate (class Authenticate, readAuth)
import Conduit.Capability.LogMessages (class LogMessages, logError)
import Conduit.Capability.Navigate (class Navigate, logout)
import Control.Monad.Reader (class MonadAsk, ask)
import Data.Argonaut.Core (Json)
import Data.Bifoldable (bitraverse_)
import Data.Bitraversable (ltraverse)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Conduit.Data.Username (Username)
import Effect.Aff.Class (class MonadAff)

-- Helper functions that leverages several of our capabilities together to help
-- run requests that require authentication.

withUser
  :: forall m e a
   . MonadAff m 
  => LogMessages m 
  => Navigate m 
  => MonadAsk { baseUrl :: BaseURL | e } m
  => Authenticate m
  => (Maybe Username -> Json -> Either String a)
  -> (BaseURL -> Request Json)
  -> m (Either String a)
withUser decode req = do
  { baseUrl } <- ask
  readAuth >>= case _ of
    Left _ -> runRequest (decode Nothing) $ req baseUrl
    Right au -> runRequest (decode $ Just $ username au) $ req baseUrl

withAuthUser 
  :: forall m a e
   . MonadAff m 
  => LogMessages m
  => Navigate m 
  => MonadAsk { baseUrl :: BaseURL | e } m
  => Authenticate m 
  => (Maybe Username -> Json -> Either String a)
  -> (AuthUser -> BaseURL -> Request Json)
  -> m (Either String a)
withAuthUser decode req =
  readAuth >>= case _ of
    Left err -> logError err *> pure (Left err) 
    Right au -> do
      { baseUrl } <- ask
      res <- runRequest (decode $ Just $ username au) (req au baseUrl)
      void $ ltraverse (\e -> logError e *> logout) res
      pure res

withAuthUser_ 
  :: forall m e
   . MonadAff m 
  => LogMessages m 
  => Navigate m 
  => MonadAsk { baseUrl :: BaseURL | e } m
  => Authenticate m 
  => (AuthUser -> BaseURL -> Request Json) 
  -> m Unit 
withAuthUser_ req =
  readAuth >>= bitraverse_
    (\e -> logError e *> logout)
    (\au -> ask >>= runRequest pure <<< req au <<< _.baseUrl)
