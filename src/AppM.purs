-- | A custom application monad that provides concrete implementations for our
-- | abstract capabilities.

module AppM where

import Prelude

import Affjax (Request, request)
import Affjax as RF
import Api.Endpoint (Endpoint(..))
import Api.Request (AuthType(..), get, post, runRequest, runRequest', runRequestAuth)
import Api.Request as Request
import Capability.Authenticate (class Authenticate, deleteAuth)
import Capability.LogMessages (class LogMessages)
import Capability.ManageResource (class ManageResource)
import Capability.Navigate (class Navigate, navigate)
import Capability.Now (class Now)
import Control.Monad.Reader.Trans (class MonadAsk, ReaderT, ask, asks, runReaderT)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Bifunctor (bimap, lmap)
import Data.Char.Unicode.Internal (gencatZS)
import Data.Either (Either(..))
import Data.Log (LogType(..))
import Data.Log as Log
import Data.Newtype (class Newtype)
import Data.Route (Route(..), routeCodec)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Now as Now
import Routing.Duplex (print)
import Routing.Hash (setHash)
import Test.Unit.Console as Console
import Type.Equality (class TypeEquals, from)

-- Our global environment will store read-only information available to any function 
-- with the right MonadAsk constraint.

type Env = 
  { logLevel :: LogLevel 
  , apiRoot :: String
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
    liftEffect $ case env.logLevel, Log.logType l of
      Prod, Debug -> pure unit
      _, _ -> Console.log $ Log.message l

-- We'll use local storage to load, save, and destroy credentials. In our tests,
-- we'll hardcode a particular user we have test data about in our system.

instance authenticateAppM :: Authenticate AppM where
  authenticate = const (pure (Left "not implemented"))
  readAuth = liftEffect Request.readAuthTokenFromLocalStorage
  writeAuth = liftEffect <<< Request.writeAuthTokenToLocalStorage
  deleteAuth = liftEffect Request.deleteAuthTokenFromLocalStorage
  
-- The root of our application is watching for hash changes, so to route from 
-- location to location we just need to set the hash. Logging out is more
-- involved; we need to invalidate the session.

instance navigateAppM :: Navigate AppM where
  navigate = liftEffect <<< setHash <<< print routeCodec 
  logout = do
    liftEffect Request.deleteAuthTokenFromLocalStorage 
    navigate Home

-- We have two resource classes -- one to  to manage resources that require no 
-- authentication, and another to manage resources that do require auth. We separate
-- these because we don't want to unnecessarily depend on an authentication class.

instance manageResourceAppM :: ManageResource AppM where
  register body = runRequestAuth (post NoAuth (encodeJson body) Users)
  getProfile = runRequest' <<< get NoAuth <<< Profiles
  getTags = runRequest' (get NoAuth Tags)
  getComments = runRequest' <<< get NoAuth <<< Comments
  getArticle = runRequest' <<< get NoAuth <<< Article
  getArticles = runRequest' <<< get NoAuth <<< Articles 
