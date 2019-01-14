-- | The Conduit homepage allows users to explore articles in several ways: in a personalized feed,
-- | by tag, or by viewing all articles.
module Conduit.Page.Home where

import Prelude

import Conduit.Api.Endpoint (ArticleParams, Pagination, noArticleParams)
import Conduit.Capability.Navigate (class Navigate)
import Conduit.Capability.Resource.Article (class ManageArticle, getArticles, getCurrentUserFeed)
import Conduit.Capability.Resource.Tag (class ManageTag, getAllTags)
import Conduit.Component.HTML.ArticleList (articleList, renderPagination)
import Conduit.Component.HTML.Footer (footer)
import Conduit.Component.HTML.Header (header)
import Conduit.Component.HTML.Utils (css, maybeElem, whenElem)
import Conduit.Component.Utils (guardSession)
import Conduit.Component.Part.FavoriteButton (favorite, unfavorite)
import Conduit.Data.Article (ArticleWithMetadata)
import Conduit.Data.PaginatedArray (PaginatedArray)
import Conduit.Data.Profile (Profile)
import Conduit.Data.Route (Route(..))
import Control.Monad.Reader (class MonadAsk)
import Data.Lens (Traversal')
import Data.Lens.Index (ix)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.Monoid (guard)
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Ref (Ref)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..), _Success, fromMaybe, toMaybe)
import Web.Event.Event (preventDefault)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)

type State =
  { tags :: RemoteData String (Array String)
  , articles :: RemoteData String (PaginatedArray ArticleWithMetadata)
  , tab :: Tab
  , currentUser :: Maybe Profile
  , page :: Int
  }

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
  | SelectPage Int MouseEvent a

component
  :: forall m r
   . MonadAff m
  => MonadAsk { currentUser :: Ref (Maybe Profile) | r } m
  => Navigate m
  => ManageTag m
  => ManageArticle m
  => H.Component HH.HTML Query Unit Void m
component =
  H.lifecycleComponent
    { initialState
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just $ H.action Initialize
    , finalizer: Nothing
    }
  where 
  initialState :: Unit -> State
  initialState _ =
    { tags: NotAsked
    , articles: NotAsked
    , tab: Global 
    , currentUser: Nothing
    , page: 1
    }

  eval :: Query ~> H.ComponentDSL State Query Void m
  eval = case _ of
    Initialize a -> do
      void $ H.fork $ eval $ LoadTags a
      guardSession >>= case _ of
        Nothing -> do 
          void $ H.fork $ eval $ LoadArticles noArticleParams a
        profile -> do
          void $ H.fork $ eval $ LoadFeed { limit: Just 20, offset: Nothing } a
          H.modify_ _ { currentUser = profile, tab = Feed }
      pure a

    LoadTags a -> do
      H.modify_ _ { tags = Loading}
      tags <- getAllTags
      H.modify_ _ { tags = fromMaybe tags }
      pure a

    LoadFeed params a -> do
      st <- H.modify _ { articles = Loading }
      articles <- getCurrentUserFeed params
      H.modify_ _ { articles = fromMaybe articles }
      pure a      

    LoadArticles params a -> do
      H.modify_ _ { articles = Loading }
      articles <- getArticles params
      H.modify_ _ { articles = fromMaybe articles }
      pure a      

    ShowTab thisTab a -> do
      st <- H.get
      when (thisTab /= st.tab) do
        H.modify_ _ { tab = thisTab }
        void $ H.fork $ eval case thisTab of 
          Feed -> 
            LoadFeed { limit: Just 20, offset: Nothing } a 
          Global -> 
            LoadArticles (noArticleParams { limit = Just 20 }) a
          Tag tag -> 
            LoadArticles (noArticleParams { tag = Just tag, limit = Just 20 }) a
      pure a
    
    FavoriteArticle index a -> 
      favorite (_article index) $> a

    UnfavoriteArticle index a -> 
      unfavorite (_article index) $> a
    
    SelectPage index event a -> do
      H.liftEffect $ preventDefault $ toEvent event
      st <- H.modify _ { page = index }
      let offset = Just (index * 20)
      void $ H.fork $ eval case st.tab of 
        Feed -> 
          LoadFeed { limit: Just 20, offset } a 
        Global -> 
          LoadArticles (noArticleParams { limit = Just 20, offset = offset }) a
        Tag tag -> 
          LoadArticles (noArticleParams { tag = Just tag, limit = Just 20, offset = offset }) a
      pure a
  
  _article :: Int -> Traversal' State ArticleWithMetadata
  _article i = 
    prop (SProxy :: SProxy "articles") 
      <<< _Success 
      <<< prop (SProxy :: SProxy "body") 
      <<< ix i

  render :: State -> H.ComponentHTML Query
  render state@{ tags, articles, currentUser } =
    HH.div_
    [ header currentUser Home
    , HH.div
      [ css "home-page" ]
      [ whenElem (isNothing currentUser) \_ -> banner
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
        [ whenElem (isJust state.currentUser) \_ -> tab state Feed
        , tab state Global
        , whenElem (tabIsTag state.tab) \_ -> tab state state.tab
        ]
      ]
    , articleList FavoriteArticle UnfavoriteArticle state.articles
    , maybeElem (toMaybe state.articles) \paginated ->  
        renderPagination SelectPage state.page paginated
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
