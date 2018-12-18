module Conduit.Component.Page.Home where

import Prelude

import Conduit.Api.Endpoint (ArticleParams, noArticleParams)
import Conduit.Api.Request (AuthUser)
import Conduit.Capability.ManageResource (class ManageResource, getArticles, getTags)
import Conduit.Capability.Navigate (class Navigate, navigate)
import Conduit.Component.HTML.ArticleList (articleList)
import Conduit.Component.HTML.Footer (footer)
import Conduit.Component.HTML.Header (header)
import Conduit.Component.HTML.Utils (css, guardHtml)
import Control.Parallel (parTraverse_)
import Conduit.Data.Article (Article)
import Data.Const (Const)
import Data.Either (either)
import Data.Maybe (Maybe(..), isJust)
import Conduit.Data.Route (Route(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..))
import Web.UIEvent.MouseEvent.EventTypes (click)

type State =
  { tags :: RemoteData String (Array String)
  , articles :: RemoteData String (Array Article)
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

data Query a
  = Initialize a
  | LoadTags a
  | Navigate Route a
  | ShowTab Tab a
  | LoadArticles ArticleParams a

component
  :: forall m
   . MonadAff m
  => ManageResource m
  => Navigate m
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
    , tab: Global 
    , authUser
    }

  eval :: Query ~> H.ParentDSL State Query (Const Void) Void Void m
  eval = case _ of
    Initialize a -> do
      parTraverse_ H.fork
        [ eval $ LoadTags a
        , eval $ LoadArticles noArticleParams a
        ]
      pure a

    LoadTags a -> do
      H.modify_ _ { tags = Loading}
      tags <- getTags
      H.modify_ _ { tags = either Failure Success tags }
      pure a

    LoadArticles params a -> do
      H.modify_ _ { articles = Loading }
      articles <- getArticles params
      H.modify_ _ { articles = either Failure Success articles }
      pure a      

    Navigate route a -> do
      navigate route
      pure a

    ShowTab tab a -> do
      st <- H.get
      when (tab /= st.tab) do
        H.modify_ _ { tab = tab }
        void case tab of 
          Feed -> pure a -- TODO
          Global -> eval $ LoadArticles noArticleParams a
          Tag tag -> eval $ LoadArticles (noArticleParams { tag = Just tag }) a
      pure a

  render :: State -> H.ParentHTML Query (Const Void) Void m
  render state@{ tags, articles, authUser } =
    HH.div_
    [ header authUser Home Navigate
    , HH.div
      [ css "home-page" ]
      [ banner
      , HH.div
        [ css "container page" ]
        [ HH.div
          [ css "row" ]
          [ mainView state
          , HH.div
            [ css "col-md-3" ]
            [ HH.div
              [ css "sidebar" ]
              [ HH.p_ [ HH.text "Popular Tags" ]
              , renderTags tags
              ]
            ]
          ]
        ]
      ]
    , footer
    ]

  banner :: forall p i. HH.HTML p i
  banner =
    HH.div
    [ css "banner" ]
    [ HH.div
      [ css "container" ]
      [ HH.h1
        [ css "logo-font" ]
        [ HH.text "conduit" ]
      , HH.p_ [ HH.text "A place to share your knowledge." ]
      ]
    ]

  mainView :: forall p. State -> H.HTML p Query
  mainView state =
    HH.div
    [ css "col-md-9" ]
    [ HH.div
      [ css "feed-toggle" ]
      [ HH.ul
        [ css "nav nav-pills outline-active" ]
        [ whenElem (isJust state.authUser) \_ -> 
            yourFeedTab state.tab
        , globalFeedTab state.tab 
        , whenElem (isJust state.authUser) \_ -> 
            tagFilterTab state.tab
        ]
      ]
    , articleList Navigate state.articles
    ]

  yourFeedTab :: forall p i. Tab -> HH.HTML p i
  yourFeedTab tab =
    HH.li
    [ css "nav-item" ]
    [ HH.a
      [ css $ "nav-link" <> modifiers ]
      [ HH.text "Your Feed" ]
    ]
    where
      modifiers = case tab of
        Feed -> " active"
        _ -> ""
  
  globalFeedTab :: forall p. Tab -> H.HTML p Query
  globalFeedTab tab =
    HH.li
    [ css "nav-item" ]
    [ HH.a
      [ css $ "nav-link" <> modifiers
      , HE.handler click $ HE.input_ $ ShowTab Global
      ]
      [ HH.text "Global Feed" ]
    ]
    where
      modifiers = case tab of
        Global -> " active"
        _ -> ""


  tagFilterTab :: forall p i. Tab -> HH.HTML p i
  tagFilterTab = case _ of
    Tag tag ->
      HH.li
      [ css "nav-item" ]
      [ HH.a
        [ css "navLink active" ]
        [ HH.i [ HP.class_ $ H.ClassName "ion-pound" ] []
        , HH.text $ "\160" <> tag
        ]
      ]

    _ -> HH.div_ []
  
  
  renderTags :: forall p. RemoteData String (Array String) -> H.HTML p Query
  renderTags = case _ of
    NotAsked ->  HH.div_ [ HH.text "Tags not loaded" ]
    Loading -> HH.div_ [ HH.text "Loading Tags" ]
    Failure err ->  HH.div_ [ HH.text $ "Failed loading tags: " <> err ]
    Success tags ->
      HH.div
      [ css "tag-list" ]
      $ tags <#> renderTag

  renderTag :: forall p. String -> H.HTML p Query
  renderTag tag =
    HH.a
    [ css "tag-default tag-pill"
    , HE.handler click $ HE.input_ $ ShowTab (Tag tag)
    ]
    [ HH.text tag ]
