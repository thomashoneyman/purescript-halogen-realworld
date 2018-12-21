module Conduit.Page.ViewArticle where

import Prelude

import Conduit.Api.Request (AuthUser)
import Conduit.Api.Request as AuthUser
import Conduit.Capability.LogMessages (class LogMessages)
import Conduit.Capability.ManageResource (class ManageAuthResource, class ManageResource, deleteArticle, getArticle)
import Conduit.Capability.Navigate (class Navigate, navigate)
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
import Data.Foldable (for_)
import Data.Formatter.DateTime (formatDateTime)
import Data.Lens (Traversal', preview)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
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
  | DeleteArticle a

type ChildQuery = RawHTML.Query
type ChildSlot = Unit

component
  :: forall m
   . MonadAff m
  => ManageResource m
  => ManageAuthResource m
  => LogMessages m
  => Navigate m
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
      follow _author $> a

    UnfollowAuthor a -> 
      unfollow _author $> a

    FavoriteArticle a -> 
      favorite _article $> a

    UnfavoriteArticle a -> 
      unfavorite _article $> a

    DeleteArticle a -> do
      st <- H.get
      for_ (preview _Success st.article) (deleteArticle <<< _.slug) 
      navigate Home
      pure a
  
  _author :: Traversal' State Author
  _author = _article <<< prop (SProxy :: SProxy "author")

  _article :: Traversal' State ArticleWithMetadata
  _article = prop (SProxy :: SProxy "article") <<< _Success

  render :: State -> H.ParentHTML Query ChildQuery Unit m
  render state =
    HH.div
      [ css "article-page" ]
      [ header state.authUser (ViewArticle state.slug)
      , maybeElem mbArticle banner
      , maybeElem mbArticle content 
      , footer
      ]
    where
    mbArticle = preview _Success state.article
    markdown = fromMaybe "Failed to load article!" (_.body <$> mbArticle)

    banner article = 
      HH.div
        [ css "banner"]
        [ HH.div 
          [ css "container" ]
          [ HH.h1_ 
              [ HH.text article.title ]
          , articleMeta article
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
            [ HH.text $ fromMaybe "" $ hush $ formatDateTime "MMMM DD, YYYY" $ PDT.toDateTime article.createdAt ]
          ]
        , case state.authUser of
            Just au | AuthUser.username au == username ->
              HH.span_
                [ HH.a
                  [ css "btn btn-outline-secondary btn-sm" 
                  , safeHref $ EditArticle article.slug
                  ]
                  [ HH.i 
                    [ css "ion-edit" ]
                    []
                  , HH.text " Edit Article"
                  ]
                , HH.text " "
                , HH.button
                  [ css "btn btn-outline-danger btn-sm" 
                  , HE.onClick $ HE.input_ DeleteArticle
                  ]
                  [ HH.i 
                    [ css "ion-trash-a" ]
                    [ ]
                  , HH.text " Delete Article"
                  ]
                ]
            _ -> 
              HH.span_
                [ followButton FollowAuthor UnfollowAuthor article.author 
                , HH.text " "
                , favoriteButton Medium FavoriteArticle UnfavoriteArticle article
                ]
        ]
      where
      username = Author.username article.author
      avatar = (Author.profile article.author).image

    content article =
      HH.div
        [ css "container page" ]
        [ HH.div
          [ css "col-xs-12" ]
          [ HH.slot unit RawHTML.component { markdown } absurd 
          , HH.ul  
            [ css "tag-list" ]
            (renderTag <$> article.tagList)
          , HH.hr_
          , HH.div
            [ css "article-actions" ]
            [ articleMeta article ]
          ]
        ]
      where
      renderTag str = 
        HH.li 
          [ css "tag-default tag-pill tag-outline" ] 
          [ HH.text str ]
      

