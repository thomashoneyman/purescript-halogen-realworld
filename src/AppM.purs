-- | A custom application monad that provides concrete implementations for our
-- | abstract capabilities.

module Conduit.AppM where

import Prelude

import Conduit.Api.Endpoint (Endpoint(..), noArticleParams)
import Conduit.Api.Request (AuthType(..), BaseURL, delete, get, post, put, runRequest)
import Conduit.Api.Request as Request
import Conduit.Api.Utils (withAuthUser, withAuthUser_, withUser)
import Conduit.Capability.Authenticate (class Authenticate, writeAuth)
import Conduit.Capability.LogMessages (class LogMessages, logError)
import Conduit.Capability.ManageResource (class ManageAuthResource, class ManageResource)
import Conduit.Capability.Navigate (class Navigate, navigate)
import Conduit.Capability.Now (class Now)
import Conduit.Data.Article (decodeArticleWithMetadata, decodeArticles)
import Conduit.Data.Author (decodeAuthorProfile)
import Conduit.Data.Comment (decodeComment, decodeComments)
import Conduit.Data.Log (LogType(..))
import Conduit.Data.Log as Log
import Conduit.Data.Profile (decodeProfileWithEmail, encodeUpdateProfile)
import Conduit.Data.Route as Route
import Control.Monad.Reader.Trans (class MonadAsk, ReaderT, ask, asks, runReaderT)
import Data.Argonaut.Decode (decodeJson, (.:))
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
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
  , baseUrl :: BaseURL
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
  authenticate fields = do 
    { baseUrl } <- ask
    liftAff (Request.login fields baseUrl) >>= case _ of
      Left err -> logError err *> pure (Left err)
      Right (Tuple au prof) -> writeAuth au *> pure (Right prof)
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
    { baseUrl } <- ask
    liftAff (Request.register body baseUrl) >>= case _ of
      Left err -> logError err *> pure (Left err)
      Right (Tuple au prof) -> writeAuth au *> pure (Right prof) 
  getTags = do
    let tagDecoder = (_ .: "tags") <=< decodeJson
    { baseUrl } <- ask
    runRequest tagDecoder $ get NoAuth Tags baseUrl
  getAuthor u = 
    withUser decodeAuthorProfile $ get NoAuth $ Profiles u
  getComments u = 
    withUser decodeComments $ get NoAuth $ Comments u
  getArticle slug =
    withUser decodeArticleWithMetadata $ get NoAuth $ Article slug
  getArticles params = 
    withUser decodeArticles $ get NoAuth $ Articles params

-- Our second resource class describes resources that do require authentication.

instance manageAuthResourceAppM :: ManageAuthResource AppM where
  getUser = 
    withAuthUser (const decodeProfileWithEmail) \t -> 
      get (Auth t) User
  updateUser p = 
    withAuthUser_ \t -> 
      put (Auth t) ( encodeUpdateProfile p) User
  followUser u = 
    withAuthUser decodeAuthorProfile \t -> 
      post (Auth t) Nothing (Follow u)
  unfollowUser u = 
    withAuthUser decodeAuthorProfile \t -> 
      delete (Auth t) (Follow u)
  createArticle article = 
    withAuthUser decodeArticleWithMetadata \t -> 
      post (Auth t) (Just $ encodeJson { article }) (Articles noArticleParams)
  updateArticle s article = 
    withAuthUser decodeArticleWithMetadata \t -> 
      put (Auth t) (encodeJson { article }) (Article s)
  deleteArticle s = 
    withAuthUser_ \t -> 
      delete (Auth t) (Article s)
  createComment s comment = 
    withAuthUser (\x -> decodeComment x <=< (_ .: "comment") <=< decodeJson) \t -> 
      post (Auth t) (Just $ encodeJson { comment }) (Comments s)
  deleteComment s cid = 
    withAuthUser_ \t -> 
      delete (Auth t) (Comment s cid)
  favoriteArticle s = 
    withAuthUser decodeArticleWithMetadata \t -> 
      post (Auth t) Nothing (Favorite s)
  unfavoriteArticle s = 
    withAuthUser decodeArticleWithMetadata \t -> 
      delete (Auth t) (Favorite s)
  getFeed p = 
    withAuthUser decodeArticles \t -> 
      get (Auth t) (Feed p)
