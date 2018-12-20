module Conduit.Page.ViewArticle where

import Prelude

import Conduit.Api.Request (AuthUser)
import Conduit.Api.Request as AuthUser
import Conduit.Capability.LogMessages (class LogMessages)
import Conduit.Capability.ManageResource (class ManageAuthResource, class ManageResource, getArticle)
import Conduit.Component.HTML.Footer (footer)
import Conduit.Component.HTML.Header (header)
import Conduit.Component.HTML.Utils (css, maybeElem, safeHref)
import Conduit.Component.Part.FavoriteButton (ButtonSize(..), favorite, favoriteButton, unfavorite)
import Conduit.Component.Part.FollowButton (follow, followButton, unfollow)
import Conduit.Component.RawHTML as RawHTML
import Conduit.Data.Article (ArticleWithMetadata)
import Conduit.Data.Author (Author)
import Conduit.Data.Author as Author
import Conduit.Data.Avatar as Avatar
import Conduit.Data.PreciseDateTime as PDT
import Conduit.Data.Route (Route(..))
import Conduit.Data.Username as Username
import Data.Either (either, hush)
import Data.Formatter.DateTime (formatDateTime)
import Data.Lens (Traversal', preview)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..), _Success)
import Slug (Slug)

type State =
  { article :: RemoteData String ArticleWithMetadata
  , slug :: Slug
  , authUser :: Maybe AuthUser
  }

type Input =
  { slug :: Slug
  , authUser :: Maybe AuthUser
  }

data Query a
  = GetArticle a
  | FollowAuthor a
  | UnfollowAuthor a
  | FavoriteArticle a
  | UnfavoriteArticle a

type ChildQuery = RawHTML.Query
type ChildSlot = Unit

component
  :: forall m
   . MonadAff m
  => ManageResource m
  => ManageAuthResource m
  => LogMessages m
  => H.Component HH.HTML Query Input Void m
component =
  H.lifecycleParentComponent
    { initialState
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just $ H.action GetArticle
    , finalizer: Nothing
    }

  where 

  initialState :: Input -> State
  initialState { slug, authUser } = { article: NotAsked, slug, authUser }

  eval :: Query ~> H.ParentDSL State Query ChildQuery Unit Void m
  eval = case _ of
    GetArticle a -> do
      st <- H.modify _ { article = Loading }
      article <- getArticle st.slug
      H.modify_ _ { article = either Failure Success article }
      pure a      

    FollowAuthor a -> 
      follow _author 
        *> eval (GetArticle a)

    UnfollowAuthor a -> 
      unfollow _author 
        *> eval (GetArticle a)

    FavoriteArticle a -> 
      favorite _article 
        *> eval (GetArticle a)

    UnfavoriteArticle a -> 
      unfavorite _article 
        *> eval (GetArticle a)
  
  _author :: Traversal' State Author
  _author = _article <<< prop (SProxy :: SProxy "author")

  _article :: Traversal' State ArticleWithMetadata
  _article = prop (SProxy :: SProxy "article") <<< _Success

  render :: State -> H.ParentHTML Query ChildQuery Unit m
  render state =
    HH.div
      [ css "article-page" ]
      [ header state.authUser (ViewArticle state.slug)
      , banner
      , HH.slot unit RawHTML.component { markdown } absurd
      , footer
      ]
    where
    mbArticle = preview _Success state.article
    markdown = fromMaybe "Failed to load article!" (_.body <$> mbArticle)

    banner = 
      HH.div
        [ css "banner"]
        [ HH.div 
          [ css "container" ]
          [ maybeElem mbArticle \a -> 
              HH.h1_ 
                [ HH.text a.title ]
          , maybeElem mbArticle articleMeta 
          ]
        ]
    
    articleMeta article =
      HH.div
        [ css "article-meta" ]
        [ HH.a 
          [ safeHref $ Profile username ]
          [ HH.img 
            [ HP.src $ Avatar.toStringWithDefault avatar ]
          ]
        , HH.div
          [ css "info" ]
          [ HH.a 
            [ css "author" 
            , safeHref $ Profile username
            ]
            [ HH.text $ Username.toString username ]
          , HH.span
            [ css "date" ]
            [ HH.text $ fromMaybe "" $ hush $ formatDateTime "MM DD YYYY" $ PDT.toDateTime article.createdAt ]
          ]
        , case state.authUser of
            Just au | AuthUser.username au == username ->
              HH.text "" -- maybeElem authUser \au ->
            _ -> 
              HH.span_
                [ followButton FollowAuthor UnfollowAuthor article.author 
                , favoriteButton Medium FavoriteArticle UnfavoriteArticle article
                ]
        ]
      where
      username = Author.username article.author
      avatar = (Author.profile article.author).image

