-- | A custom application monad that provides concrete implementations for our
-- | abstract capabilities.

module AppM where

import Prelude

import Affjax (Request)
import Api.Endpoint (Endpoint(..), noArticleParams)
import Api.Request (AuthType(..), AuthUser, URL, delete, get, post, put, runRequest, username)
import Api.Request as Request
import Capability.Authenticate (class Authenticate, deleteAuth, readAuth, writeAuth)
import Capability.LogMessages (class LogMessages, logError)
import Capability.ManageResource (class ManageAuthResource, class ManageResource)
import Capability.Navigate (class Navigate, logout, navigate)
import Capability.Now (class Now)
import Control.Monad.Reader.Trans (class MonadAsk, ReaderT, ask, asks, runReaderT)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson, (.?))
import Data.Argonaut.Encode (encodeJson)
import Data.Article (decodeArticle, decodeArticles)
import Data.Author (decodeAuthor)
import Data.Bitraversable (bitraverse_, ltraverse)
import Data.Comment (decodeComment, decodeComments)
import Data.Either (Either(..))
import Data.Log (LogType(..))
import Data.Log as Log
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Route as Route
import Data.Tuple (Tuple(..))
import Data.Username (Username)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Effect.Now as Now
import Routing.Duplex (print)
import Routing.Hash (setHash)
import Type.Equality (class TypeEquals, from)

-- Our global environment will store read-only information available to any function 
-- with the right MonadAsk constraint.

type Env = 
  { logLevel :: LogLevel 
  , rootUrl :: URL
  }

data LogLevel = Dev | Prod

derive instance eqLogLevel :: Eq LogLevel
derive instance ordLogLevel :: Ord LogLevel

-- Our application monad will be the base of a `ReaderT` transformer. We gain the 
-- functionality of `ReaderT` in addition to any other instances we write. Our
-- instances will provide the implementations for our capabilities.

newtype AppM a = AppM (ReaderT Env Aff a)

-- We'll write the instances below, but first, we'll write a function that will 
-- recover `Aff` from our custom type. Halogen applications must run in `Aff`, 
-- so this is a necessary step. See its use in `Main.purs` for more.

runAppM :: Env -> AppM ~> Aff
runAppM env (AppM m) = runReaderT m env

-- We can  derive all the instances we need to make this new type a monad and allow 
-- the use of `Effect` and `Aff`.

derive instance newtypeAppM :: Newtype (AppM a) _

derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM

-- This instance has to be defined manually, as we are using a type synonym 
-- rather than a newtype for our environment.

instance monadAskAppM :: TypeEquals e Env => MonadAsk e AppM where
  ask = AppM $ asks from

-- Next, we'll define instances for each of our capabilities.

-- We'll rely on the Effect instances to fetch date times, so this instance is
-- trivial. However, relying on this instance means we can easily swap in another
-- time source. Our tests will rely on static values, for example.

instance nowAppM :: Now AppM where
  now = liftEffect Now.now
  nowDate = liftEffect Now.nowDate
  nowTime = liftEffect Now.nowTime
  nowDateTime = liftEffect Now.nowDateTime

-- We will ultimately log messages to a logging service, but for now, we'll only
-- log to the console. We'll log everything in development, but we'll only log
-- warnings and errors in production.

instance logMessagesAppM :: LogMessages AppM where
  logMessage l = do 
    env <- ask
    liftEffect case env.logLevel, Log.logType l of
      Prod, Debug -> pure unit
      _, _ -> Console.log $ Log.message l

-- We'll use local storage to load, save, and destroy credentials. In our tests,
-- we'll hardcode a particular user we have test data about in our system.

instance authenticateAppM :: Authenticate AppM where
  authenticate fields = ask >>= \env -> Request.login env.rootUrl fields
  readAuth = liftEffect Request.readAuthUserFromLocalStorage
  writeAuth = liftEffect <<< Request.writeAuthUserToLocalStorage
  deleteAuth = liftEffect Request.deleteAuthUserFromLocalStorage
  
-- The root of our application is watching for hash changes, so to route from 
-- location to location we just need to set the hash. Logging out is more
-- involved; we need to invalidate the session.

instance navigateAppM :: Navigate AppM where
  navigate = liftEffect <<< setHash <<< print Route.routeCodec 
  logout = do
    liftEffect Request.deleteAuthUserFromLocalStorage 
    navigate Route.Home

-- Our first resource class describes resources that do not require any authentication.

instance manageResourceAppM :: ManageResource AppM where
  register body = do
    { rootUrl } <- ask
    liftAff (Request.register rootUrl body) >>= case _ of
      Left err -> logError err *> pure (Left err)
      Right (Tuple au prof) -> writeAuth au *> pure (Right prof) 
  getTags = do
    let tagDecoder = (_ .? "tags") <=< decodeJson
    { rootUrl } <- ask
    runRequest tagDecoder $ get NoAuth rootUrl Tags
  getProfile u = 
    withUser decodeAuthor $ \url -> get NoAuth url $ Profiles u
  getComments u = 
    withUser decodeComments $ \url -> get NoAuth url $ Comments u
  getArticle slug = 
    withUser decodeArticle $ \url -> get NoAuth url $ Article slug
  getArticles params = 
    withUser decodeArticles $ \url -> get NoAuth url $ Articles params

-- Our second resource class describes resources that do require authentication.

instance manageAuthResourceAppM :: ManageAuthResource AppM where
  getUser = 
    withAuthUser (const decodeJson) \t url -> get (Auth t) url Users
  updateUser p = 
    withAuthUser_ \t url -> post (Auth t) (Just $ encodeJson p) url Users
  followUser u = 
    withAuthUser decodeAuthor \t url -> post (Auth t) Nothing url (Follow u)
  unfollowUser u = 
    withAuthUser decodeAuthor \t url -> delete (Auth t) url (Follow u)
  createArticle a = 
    withAuthUser decodeArticle \t url -> post (Auth t) (Just $ encodeJson a) url (Articles noArticleParams)
  updateArticle s a = 
    withAuthUser decodeArticle \t url -> put (Auth t) (encodeJson a) url (Article s)
  deleteArticle s = 
    withAuthUser_ \t url -> delete (Auth t) url (Article s)
  createComment s c = 
    withAuthUser decodeComment \t url -> post (Auth t) (Just $ encodeJson c) url (Comments s)
  deleteComment s cid = 
    withAuthUser_ \t url -> delete (Auth t) url (Comment s cid)
  favoriteArticle s = 
    withAuthUser decodeArticle \t url -> post (Auth t) Nothing url (Favorite s)
  unfavoriteArticle s = 
    withAuthUser decodeArticle \t url -> delete (Auth t) url (Favorite s)
  getFeed p = 
    withAuthUser decodeArticles \t url -> get (Auth t) url (Feed p)

-- A helper function that leverages several of our capabilities together to help
-- run requests that require authentication.

withUser
  :: forall m a
   . MonadAff m 
  => LogMessages m 
  => Navigate m 
  => MonadAsk Env m
  => Authenticate m
  => (Username -> Json -> Either String a)
  -> (URL -> Request Json)
  -> m (Either String a)
withUser decode req =
  readAuth >>= case _ of
    Left err -> logError err *> pure (Left err)
    Right au -> do
      { rootUrl } <- ask
      runRequest (decode (username au)) $ req rootUrl

withAuthUser 
  :: forall m a 
   . MonadAff m 
  => LogMessages m
  => Navigate m 
  => MonadAsk Env m
  => Authenticate m 
  => (Username -> Json -> Either String a)
  -> (AuthUser -> URL -> Request Json)
  -> m (Either String a)
withAuthUser decode req =
  readAuth >>= case _ of
    Left err -> logError err *> deleteAuth *> navigate Route.Login *> pure (Left err) 
    Right au -> do
      { rootUrl } <- ask
      res <- runRequest (decode (username au)) (req au rootUrl)
      void $ ltraverse (\e -> logError e *> logout) res
      pure res

-- A helper function that leverages several of our capabilities together to help
-- run requests that require authentication.

withAuthUser_ 
  :: forall m
   . MonadAff m 
  => LogMessages m 
  => Navigate m 
  => MonadAsk Env m
  => Authenticate m 
  => (AuthUser -> URL -> Request Json) 
  -> m Unit 
withAuthUser_ req =
  readAuth >>= bitraverse_ (\e -> logError e *> logout) (\au -> do
                                                            { rootUrl } <- ask
                                                            runRequest pure $ req au rootUrl)
