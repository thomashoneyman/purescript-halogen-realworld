-- | A custom application monad that provides concrete implementations for capabilities like
-- | logging, navigation, and resource management. This is our production monad -- it interprets
-- | our capabilities as they are meant to run on our production site.
-- |
-- | However, since capabilities like logging are implemented as type classes, we can also provide
-- | one or more test monads that provide different interpretations.
-- |
-- | For example, this monad will actually hit the server with API requests when we manage a
-- | resource, but our test monad might just return mock JSON or error responses.
-- |
-- | See the various `Conduit.Capability.*` modules for deeper explanations of each capability, and
-- | the accompanying guide for a thorough introduction to this style of application architecture.
-- |
-- | https://thomashoneyman.com/guides/real-world-halogen
module Conduit.AppM where

import Prelude

import Conduit.Api.Endpoint (Endpoint(..), noArticleParams)
import Conduit.Api.Request (RequestMethod(..))
import Conduit.Api.Request as Request
import Conduit.Api.Utils (authenticate, decode, decodeWithUser, mkAuthRequest, mkRequest)
import Conduit.Capability.LogMessages (class LogMessages)
import Conduit.Capability.Navigate (class Navigate, navigate)
import Conduit.Capability.Now (class Now)
import Conduit.Capability.Resource.Article (class ManageArticle)
import Conduit.Capability.Resource.Comment (class ManageComment)
import Conduit.Capability.Resource.Tag (class ManageTag)
import Conduit.Capability.Resource.User (class ManageUser)
import Conduit.Data.Article as Article
import Conduit.Data.Comment as Comment
import Conduit.Data.Log as Log
import Conduit.Data.Profile as Profile
import Conduit.Data.Route as Route
import Conduit.Store (Action(..), LogLevel(..), Store)
import Conduit.Store as Store
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut as Codec
import Data.Codec.Argonaut.Record as CAR
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Effect.Now as Now
import Halogen as H
import Halogen.Store.Monad (class MonadStore, StoreT, getStore, runStoreT, updateStore)
import Routing.Duplex (print)
import Routing.Hash (setHash)
import Safe.Coerce (coerce)

-- | In the capability modules (`Conduit.Capability.*`), we wrote some abstract, high-level
-- | interfaces for business logic that tends to be highly effectful, like resource management and
-- | logging. We wrote interfaces (just the types, no actual implementation) so that we could write
-- | the same code once and swap in different implementations as we see fit.
-- |
-- | This carries two main benefits. First, it helps abstract away the implementation so we can
-- | focus on logic: businesses care about reading, writing, and deleting resources, not whether
-- | that's done on the file system, over RPC, a REST API, or something else. Second, it lets us
-- | write code once and swap in different implementations under the hood. That means we can run
-- | the same section of code in production using one implementation and as part of a test suite
-- | using another. For example, our production code might use a REST API, but our test code might
-- | just provide mock JSON responses.
-- |

-- | This module implements a monad that can run all the abstract capabilities we've defined. This
-- | is our production monad. We'll implement the monad first, and then we'll provide concrete
-- | instances for each of our abstract capabilities.
-- |
-- | The `Aff` monad allows us to run asynchronous effects. When you're in `Aff`, you can write
-- | code that makes API requests, writes files, and so on. In addition, using the `liftEffect`
-- | function, you can use any function that relies on the `Effect` monad in `Aff`. For example,
-- | you can log messages to the console within `Aff` using (liftEffect <<< Console.log). It's
-- | powerful stuff.
-- |
-- | The `StoreT` monad transformer adds the ability to have a central state in our Halogen
-- | application on top of the abilities of `Aff`. See the `halogen-store` library for more
-- | details!
-- |
-- | `AppM` combines the `Aff` and `Store` monads under a new type, which we can now use to write
-- | instances for our capabilities. We're able to combine these monads because `StoreT` is a
-- | monad transformer. Monad transformers are too large a topic to delve into here; for now, it's
-- | enough to know that they let you combine the abilities of two or more monads.
newtype AppM a = AppM (StoreT Store.Action Store.Store Aff a)

runAppM :: forall q i o. Store.Store -> H.Component q i o AppM -> Aff (H.Component q i o Aff)
runAppM store = runStoreT store Store.reduce <<< coerce

-- | We can get a monad out of our `AppM` type essentially for free by deferring to the underlying
-- | `ReaderT` instances. PureScript allows any newtype to re-use the type class instances of the
-- | type it wraps with the `derive newtype instance` syntax. It's as if the newtype didn't exist
-- | and the function was being applied to the type underneath directly.
-- |
-- | To be a monad, a type must implement the `Functor`, `Apply`, `Applicative`, `Bind`, and
-- | `Monad` type classes.
-- |
-- | In addition, because we used `Aff` as the base of our custom monad, we can also derive
-- | `MonadEffect` and `MonadAff`, two type classes that let us use any functions that run
-- | in `Effect` or in `Aff`. Having access to these two type classes lets us perform pretty much
-- | any effect we see fit, from API requests to local storage access.
-- |
-- | Finally, since we're using the `halogen-store` library, we can also derive a `MonadStore`
-- | constraint that lets us use our central state anywhere in the application.
-- |
-- | With the compiler by your side, you don't need to know how to implement a monad from scratch.
-- | You can derive everything you need! We can now focus just on the instances that matter to us:
-- | our app environment and our capabilities.
derive newtype instance functorAppM :: Functor AppM

derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM
derive newtype instance monadStoreAppM :: MonadStore Action Store AppM

-- | We're finally ready to write concrete implementations for each of our abstract capabilities.
-- | For an in-depth description of each capability, please refer to the relevant `Capability.*`
-- | module for the capability that interests you.

-- | First up: the `Now` capability, which allows us to retrieve the current time. In our
-- | production monad, we'll rely on existing functions that run in the `Effect` (synchronous
-- | effects) monad and use the `liftEffect` function so that they run in `AppM` instead. This is
-- | made possible because our `AppM` type implements the `MonadEffect` type class.
-- |
-- | ```purescript
-- | liftEffect :: forall a m. MonadEffect m => Effect a -> m a
-- | ```
-- |
-- | In our test monad, we won't perform effects -- we'll just return a hard-coded time so that we
-- | can ensure tests are reproducible.
instance nowAppM :: Now AppM where
  now = liftEffect Now.now
  nowDate = liftEffect Now.nowDate
  nowTime = liftEffect Now.nowTime
  nowDateTime = liftEffect Now.nowDateTime

-- | Next up: logging. Ideally we'd use a logging service, but for the time being, we'll just log
-- | to the console. We'll rely on our global environment to decide whether to log all messages
-- | (`Dev`) or just important messages (`Prod`).
instance logMessagesAppM :: LogMessages AppM where
  logMessage log = do
    { logLevel } <- getStore
    liftEffect case logLevel, Log.reason log of
      Prod, Log.Debug -> pure unit
      _, _ -> Console.log $ Log.message log

-- | Our app uses hash-based routing, so to navigate from place to place, we'll just set the hash.
-- | Note how our navigation capability uses our routing data type rather than let you set any
-- | arbitrary hash. Logging out is a little more involved, because we need to clean up things like
-- | the auth token. Navigating home will take care of emptying the reference to the current user.
instance navigateAppM :: Navigate AppM where
  navigate =
    liftEffect <<< setHash <<< print Route.routeCodec

  logout = do
    liftEffect $ Request.removeToken
    updateStore LogoutUser
    navigate Route.Home

-- | Our first resource class describes what operations we have available to manage users. Logging
-- | in and registration require manipulating a token, but we've designed the `Token` type so its
-- | contents can't be read by any function outside the `Api.Request` module. For that reason,
-- | the `login` and `register` implementations are directly imported. The others use our nicer
-- | `mkRequest` and `mkAuthRequest` helpers.
instance manageUserAppM :: ManageUser AppM where
  loginUser =
    authenticate Request.login

  registerUser =
    authenticate Request.register

  getCurrentUser = do
    mbJson <- mkAuthRequest { endpoint: User, method: Get }
    map (map _.user)
      $ decode (CAR.object "User" { user: Profile.profileWithEmailCodec }) mbJson

  getAuthor username = do
    mbJson <- mkRequest { endpoint: Profiles username, method: Get }
    map (map _.profile)
      $ decodeWithUser (\u -> CAR.object "Profile" { profile: Profile.authorCodec u }) mbJson

  updateUser user = do
    let
      codec = CAR.object "User" { user: Profile.profileWithEmailPasswordCodec }
      method = Put $ Just $ Codec.encode codec { user }

    void $ mkAuthRequest { endpoint: User, method: method }

  followUser username = do
    mbJson <- mkAuthRequest { endpoint: Follow username, method: Post Nothing }
    map (map _.profile)
      $ decodeWithUser (\u -> CAR.object "Profile" { profile: Profile.authorCodec u }) mbJson

  unfollowUser username = do
    mbJson <- mkAuthRequest { endpoint: Follow username, method: Delete }
    map (map _.profile)
      $ decodeWithUser (\u -> CAR.object "Profile" { profile: Profile.authorCodec u }) mbJson

-- | Our operations for managing tags
instance manageTagAppM :: ManageTag AppM where
  getAllTags = do
    mbJson <- mkRequest { endpoint: Tags, method: Get }
    map (map _.tags) $ decode (CAR.object "Tags" { tags: CA.array CA.string }) mbJson

-- | Our operations for managing comments
instance manageCommentAppM :: ManageComment AppM where
  getComments slug = do
    mbJson <- mkRequest { endpoint: Comments slug, method: Get }
    map (map _.comments)
      $ decodeWithUser (\u -> CAR.object "Comments" { comments: CA.array (Comment.commentWithMetadataCodec u) }) mbJson

  createComment slug comment = do
    let
      codec = CAR.object "Comment" { comment: Comment.commentCodec }
      method = Post $ Just $ Codec.encode codec { comment }

    mbJson <- mkAuthRequest { endpoint: Comments slug, method }
    map (map _.comment)
      $ decodeWithUser (\u -> CAR.object "Comment" { comment: Comment.commentWithMetadataCodec u }) mbJson

  deleteComment slug id =
    void $ mkAuthRequest { endpoint: Comment slug id, method: Delete }

-- | Our operations for managing articles
instance manageArticleAppM :: ManageArticle AppM where
  getArticle slug = do
    mbJson <- mkRequest { endpoint: Article slug, method: Get }
    map (map _.article)
      $ decodeWithUser (\u -> CAR.object "Article" { article: Article.articleWithMetadataCodec u }) mbJson

  getArticles fields =
    mkRequest { endpoint: Articles fields, method: Get }
      >>= decodeWithUser Article.articlesWithMetadataCodec

  createArticle article = do
    let
      codec = CAR.object "Article" { article: Article.articleCodec }
      method = Post $ Just $ Codec.encode codec { article }

    mbJson <- mkAuthRequest { endpoint: Articles noArticleParams, method }
    map (map _.article)
      $ decodeWithUser (\u -> CAR.object "Article" { article: Article.articleWithMetadataCodec u }) mbJson

  updateArticle slug article = do
    let
      codec = CAR.object "Article" { article: Article.articleCodec }
      method = Put $ Just $ Codec.encode codec { article }

    mbJson <- mkAuthRequest { endpoint: Article slug, method }
    map (map _.article) $ decodeWithUser (\u -> CAR.object "Article" { article: Article.articleWithMetadataCodec u }) mbJson

  deleteArticle slug =
    void $ mkAuthRequest { endpoint: Article slug, method: Delete }

  favoriteArticle slug = do
    mbJson <- mkAuthRequest { endpoint: Favorite slug, method: Post Nothing }
    map (map _.article) $ decodeWithUser (\u -> CAR.object "Article" { article: Article.articleWithMetadataCodec u }) mbJson

  unfavoriteArticle slug = do
    mbJson <- mkAuthRequest { endpoint: Favorite slug, method: Delete }
    map (map _.article) $ decodeWithUser (\u -> CAR.object "Article" { article: Article.articleWithMetadataCodec u }) mbJson

  getCurrentUserFeed params =
    mkAuthRequest { endpoint: Feed params, method: Get }
      >>= decodeWithUser Article.articlesWithMetadataCodec
