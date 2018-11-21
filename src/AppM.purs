-- | A custom application monad that provides concrete implementations for our
-- | abstract capabilities.

module AppM where

import Prelude

import Affjax (Request)
import Api.Endpoint (Endpoint(..), noArticleParams)
import Api.Request (AuthType(..), AuthUser, delete, get, post, put, runRequest, username)
import Api.Request as Request
import Capability.Authenticate (class Authenticate, deleteAuth, readAuth, writeAuth)
import Capability.LogMessages (class LogMessages, logError)
import Capability.ManageResource (class ManageAuthResource, class ManageResource)
import Capability.Navigate (class Navigate, logout, navigate)
import Capability.Now (class Now)
import Control.Monad.Reader.Trans (class MonadAsk, ReaderT, ask, asks, runReaderT)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson)
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
  , rootUrl :: String
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
  authenticate = Request.login
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
    liftAff (Request.register body) >>= case _ of
      Left err -> logError err *> pure (Left err)
      Right (Tuple au prof) -> writeAuth au *> pure (Right prof) 
  getTags = 
    runRequest decodeJson $ get NoAuth Tags
  getProfile u = 
    withUser decodeAuthor $ get NoAuth $ Profiles u
  getComments u = 
    withUser decodeComments $ get NoAuth $ Comments u
  getArticle slug = 
    withUser decodeArticle $ get NoAuth $ Article slug
  getArticles params = 
    withUser decodeArticles $ get NoAuth $ Articles params

-- Our second resource class describes resources that do require authentication.

instance manageAuthResourceAppM :: ManageAuthResource AppM where
  getUser = 
    withAuthUser (const decodeJson) \t -> get (Auth t) Users
  updateUser p = 
    withAuthUser_ \t -> post (Auth t) (Just $ encodeJson p) Users
  followUser u = 
    withAuthUser decodeAuthor \t -> post (Auth t) Nothing (Follow u)
  unfollowUser u = 
    withAuthUser decodeAuthor \t -> delete (Auth t) (Follow u)
  createArticle a = 
    withAuthUser decodeArticle \t -> post (Auth t) (Just $ encodeJson a) (Articles noArticleParams)
  updateArticle s a = 
    withAuthUser decodeArticle \t -> put (Auth t) (encodeJson a) (Article s)
  deleteArticle s = 
    withAuthUser_ \t -> delete (Auth t) (Article s)
  createComment s c = 
    withAuthUser decodeComment \t -> post (Auth t) (Just $ encodeJson c) (Comments s)
  deleteComment s cid = 
    withAuthUser_ \t -> delete (Auth t) (Comment s cid)
  favoriteArticle s = 
    withAuthUser decodeArticle \t -> post (Auth t) Nothing (Favorite s)
  unfavoriteArticle s = 
    withAuthUser decodeArticle \t -> delete (Auth t) (Favorite s)
  getFeed p = 
    withAuthUser decodeArticles \t -> get (Auth t) (Feed p)

-- A helper function that leverages several of our capabilities together to help
-- run requests that require authentication.

withUser
  :: forall m a
   . MonadAff m 
  => LogMessages m 
  => Navigate m 
  => Authenticate m
  => (Username -> Json -> Either String a)
  -> Request Json
  -> m (Either String a)
withUser decode req =
  readAuth >>= case _ of
    Left err -> logError err *> pure (Left err)
    Right au -> runRequest (decode (username au)) req

withAuthUser 
  :: forall m a 
   . MonadAff m 
  => LogMessages m 
  => Navigate m 
  => Authenticate m 
  => (Username -> Json -> Either String a)
  -> (AuthUser -> Request Json)
  -> m (Either String a)
withAuthUser decode req =
  readAuth >>= case _ of
    Left err -> logError err *> deleteAuth *> navigate Route.Login *> pure (Left err) 
    Right au -> do
      res <- runRequest (decode (username au)) (req au)
      void $ ltraverse (\e -> logError e *> logout) res
      pure res

-- A helper function that leverages several of our capabilities together to help
-- run requests that require authentication.

withAuthUser_ 
  :: forall m
   . MonadAff m 
  => LogMessages m 
  => Navigate m 
  => Authenticate m 
  => (AuthUser -> Request Json) 
  -> m Unit 
withAuthUser_ req =
  readAuth >>= bitraverse_ (\e -> logError e *> logout) (runRequest pure <<< req)