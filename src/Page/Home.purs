module Conduit.Page.Home where

import Prelude

import Conduit.Api.Endpoint (ArticleParams, Pagination, noArticleParams)
import Conduit.Api.Request (AuthUser)
import Conduit.Capability.LogMessages (class LogMessages)
import Conduit.Capability.ManageResource (class ManageAuthResource, class ManageResource, getArticles, getFeed, getTags)
import Conduit.Component.HTML.ArticleList (articleList)
import Conduit.Component.HTML.Footer (footer)
import Conduit.Component.HTML.Header (header)
import Conduit.Component.HTML.Utils (css, whenElem)
import Conduit.Component.Part.FavoriteButton (favorite, unfavorite)
import Conduit.Data.Article (ArticleWithMetadata)
import Conduit.Data.Route (Route(..))
import Control.Parallel (parTraverse_)
import Data.Const (Const)
import Data.Either (either)
import Data.Lens (Traversal')
import Data.Lens.Index (ix)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.Monoid (guard)
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..), _Success)

type State =
  { tags :: RemoteData String (Array String)
  , articles :: RemoteData String (Array ArticleWithMetadata)
  , tab :: Tab
  , authUser :: Maybe AuthUser
  }

type Input =
  { authUser :: Maybe AuthUser }

data Tab
  = Feed
  | Global
  | Tag String

derive instance eqTab :: Eq Tab

tabIsTag :: Tab -> Boolean
tabIsTag (Tag _) = true
tabIsTag _ = false

data Query a
  = Initialize a
  | ShowTab Tab a
  | LoadFeed Pagination a
  | LoadArticles ArticleParams a
  | LoadTags a
  | FavoriteArticle Int a
  | UnfavoriteArticle Int a

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
    , initializer: Just $ H.action Initialize
    , finalizer: Nothing
    }

  where 

  initialState :: Input -> State
  initialState { authUser } =
    { tags: NotAsked
    , articles: NotAsked
    , tab: if isJust authUser then Feed else Global 
    , authUser
    }

  eval :: Query ~> H.ParentDSL State Query (Const Void) Void Void m
  eval = case _ of
    Initialize a -> do
      st <- H.get
      parTraverse_ H.fork
        [ if isJust st.authUser 
            then eval $ LoadFeed { limit: Nothing, offset: Nothing } a
            else eval $ LoadArticles noArticleParams a
        , eval $ LoadTags a
        ]
      pure a

    LoadTags a -> do
      H.modify_ _ { tags = Loading}
      tags <- getTags
      H.modify_ _ { tags = either Failure Success tags }
      pure a

    LoadFeed params a -> do
      H.modify_ _ { articles = Loading }
      articles <- getFeed params
      H.modify_ _ { articles = either Failure Success articles }
      pure a      

    LoadArticles params a -> do
      H.modify_ _ { articles = Loading }
      articles <- getArticles params
      H.modify_ _ { articles = either Failure Success articles }
      pure a      

    ShowTab thisTab a -> do
      st <- H.get
      when (thisTab /= st.tab) do
        H.modify_ _ { tab = thisTab }
        void case thisTab of 
          Feed -> eval $ LoadFeed { limit: Nothing, offset: Nothing } a 
          Global -> eval $ LoadArticles noArticleParams a
          Tag tag -> eval $ LoadArticles (noArticleParams { tag = Just tag }) a
      pure a
    
    FavoriteArticle index a -> 
      favorite (_article index) $> a

    UnfavoriteArticle index a -> 
      unfavorite (_article index) $> a
  
  _article :: Int -> Traversal' State ArticleWithMetadata
  _article i = prop (SProxy :: SProxy "articles") <<< _Success <<< ix i

  render :: State -> H.ParentHTML Query (Const Void) Void m
  render state@{ tags, articles, authUser } =
    HH.div_
    [ header authUser Home
    , HH.div
      [ css "home-page" ]
      [ whenElem (isNothing authUser) \_ -> banner
      , HH.div
        [ css "container page" ]
        [ HH.div
          [ css "row" ]
          [ mainView state
          , HH.div
            [ css "col-md-3" ]
            [ HH.div
              [ css "sidebar" ]
              [ HH.p_ 
                [ HH.text "Popular Tags" ]
              , renderTags tags
              ]
            ]
          ]
        ]
      ]
    , footer
    ]

  mainView :: forall i. State -> H.HTML i Query
  mainView state =
    HH.div
    [ css "col-md-9" ]
    [ HH.div
      [ css "feed-toggle" ]
      [ HH.ul
        [ css "nav nav-pills outline-active" ]
        [ whenElem (isJust state.authUser) \_ -> tab state Feed
        , tab state Global
        , whenElem (tabIsTag state.tab) \_ -> tab state state.tab
        ]
      ]
    , articleList FavoriteArticle UnfavoriteArticle state.articles
    ]
  
  banner :: forall i p. HH.HTML i p 
  banner =
    HH.div
    [ css "banner" ]
    [ HH.div
      [ css "container" ]
      [ HH.h1
        [ css "logo-font" ]
        [ HH.text "conduit" ]
      , HH.p_ 
        [ HH.text "A place to share your knowledge." ]
      ]
    ]

  tab :: forall i. State -> Tab -> H.HTML i Query
  tab st thisTab =
    HH.li
      [ css "nav-item" ]
      [ HH.a
        [ css $ "nav-link" <> guard (st.tab == thisTab) " active" 
        , HE.onClick $ HE.input_ $ ShowTab thisTab 
        , HP.href "#/"
        ]
        htmlBody
      ]
    where
    htmlBody = case thisTab of
      Feed -> 
        [ HH.text "Your Feed" ]
      Global -> 
        [ HH.text "Global Feed" ]
      Tag tag ->
        [ HH.i 
          [ css "ion-pound" ] 
          []
        , HH.text $ "\160" <> tag
        ]
  
  renderTags :: forall i. RemoteData String (Array String) -> H.HTML i Query
  renderTags = case _ of
    NotAsked ->  
      HH.div_ 
        [ HH.text "Tags not loaded" ]
    Loading -> 
      HH.div_ 
        [ HH.text "Loading Tags" ]
    Failure err ->  
      HH.div_ 
        [ HH.text $ "Failed loading tags: " <> err ]
    Success tags ->
      HH.div
        [ css "tag-list" ]
        (tags <#> renderTag)

  renderTag :: forall p. String -> H.HTML p Query
  renderTag tag =
    HH.a
    [ css "tag-default tag-pill"
    , HE.onClick $ HE.input_ $ ShowTab (Tag tag)
    , HP.href "#/"
    ]
    [ HH.text tag ]
