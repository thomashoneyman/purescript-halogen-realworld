-- | A custom application monad that provides concrete implementations for our
-- | abstract capabilities.

module Conduit.AppM where

import Prelude

import Conduit.Api.Request (BaseURL, RequestMethod(..))
import Conduit.Api.Request as Request
import Conduit.Api.Utils (authenticate, decode, decodeAt, decodeWithUser, mkAuthRequest, mkRequest)
import Conduit.Capability.LogMessages (class LogMessages)
import Conduit.Capability.Navigate (class Navigate, navigate)
import Conduit.Capability.Now (class Now)
import Conduit.Capability.Resource.Article (class ManageArticle)
import Conduit.Capability.Resource.Comment (class ManageComment)
import Conduit.Capability.Resource.Tag (class ManageTag)
import Conduit.Capability.Resource.User (class ManageUser)
import Conduit.Data.Article (decodeArticle, decodeArticles)
import Conduit.Data.Author (decodeAuthor)
import Conduit.Data.Comment (decodeComments)
import Conduit.Data.Endpoint (Endpoint(..), noArticleParams)
import Conduit.Data.Log (LogType(..))
import Conduit.Data.Log as Log
import Conduit.Data.Profile (Profile)
import Conduit.Data.Route as Route
import Control.Monad.Reader.Trans (class MonadAsk, ReaderT, ask, asks, runReaderT)
import Data.Argonaut.Encode (encodeJson)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Effect.Now as Now
import Effect.Ref (Ref)
import Routing.Duplex (print)
import Routing.Hash (setHash)
import Type.Equality (class TypeEquals, from)

-- Our global environment will store read-only information available to any function 
-- with the right MonadAsk constraint.

type Env = 
  { logLevel :: LogLevel 
  , baseUrl :: BaseURL
  , currentUser :: Ref (Maybe Profile)
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

-- The root of our application is watching for hash changes, so to route from 
-- location to location we just need to set the hash. Logging out is more
-- involved; we need to invalidate the session.

instance navigateAppM :: Navigate AppM where
  navigate = 
    liftEffect <<< setHash <<< print Route.routeCodec 

  logout = do
    liftEffect Request.removeToken 
    navigate Route.Home

-- Our first resource class describes what operations we have available to manage users.
-- Logging in and registration require manipulating a token, so those are imported. All
-- other requests rely on our `mkRequest` or `mkAuthRequest` helpers.
instance manageUserAppM :: ManageUser AppM where
  loginUser = 
    authenticate Request.login

  registerUser = 
    authenticate Request.register

  getCurrentUser = 
    mkAuthRequest { endpoint: User, method: Get }
      >>= decode (decodeAt "user")

  getAuthor username = 
    mkRequest { endpoint: Profiles username, method: Get }
      >>= decodeWithUser (\mbU -> decodeAuthor mbU <=< decodeAt "profile")

  updateUser fields = 
    void $ mkAuthRequest { endpoint: User, method: Post (Just (encodeJson fields)) }

  followUser username = 
    mkAuthRequest { endpoint: Follow username, method: Post Nothing }
      >>= decodeWithUser (\mbU -> decodeAuthor mbU <=< decodeAt "profile")
  
  unfollowUser username =
    mkAuthRequest { endpoint: Follow username, method: Delete }
      >>= decodeWithUser (\mbU -> decodeAuthor mbU <=< decodeAt "profile")


-- Our operations for managing tags
instance manageTagAppM :: ManageTag AppM where 
  getAllTags = 
    mkRequest { endpoint: Tags, method: Get }
      >>= decode (decodeAt "tags")

-- Our operations for managing comments
instance manageCommentAppm :: ManageComment AppM where
  getComments slug =
    mkRequest { endpoint: Comments slug, method: Get }
      >>= decodeWithUser decodeComments
  
  createComment slug fields = 
    let method = Post $ Just $ encodeJson fields
     in void $ mkAuthRequest { endpoint: Comments slug, method }

  deleteComment slug id =
    void $ mkAuthRequest { endpoint: Comment slug id, method: Delete }

-- Our operations for managing articles 
instance manageArticleAppM :: ManageArticle AppM where
  getArticle slug =
    mkRequest { endpoint: Article slug, method: Get }
      >>= decodeWithUser decodeArticle
  
  getArticles fields =
    mkRequest { endpoint: Articles fields, method: Get }
      >>= decodeWithUser decodeArticles
  
  createArticle article = do
    let method = Post $ Just $ encodeJson { article }
    mkAuthRequest { endpoint: Articles noArticleParams, method }
      >>= decodeWithUser decodeArticle
  
  updateArticle slug article = do
    let method = Post $ Just $ encodeJson { article }
    mkAuthRequest { endpoint: Article slug, method }
      >>= decodeWithUser decodeArticle

  deleteArticle slug = 
    void $ mkAuthRequest { endpoint: Article slug, method: Delete }
  
  favoriteArticle slug = 
    mkAuthRequest { endpoint: Favorite slug, method: Post Nothing }
      >>= decodeWithUser decodeArticle

  unfavoriteArticle slug = 
    mkAuthRequest { endpoint: Favorite slug, method: Delete }
      >>= decodeWithUser decodeArticle
  
  getCurrentUserFeed params =
    mkAuthRequest { endpoint: Feed params, method: Get }
      >>= decodeWithUser decodeArticles
