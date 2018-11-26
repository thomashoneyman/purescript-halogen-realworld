module Component.Page.Home where

import Prelude

import Api.Endpoint (ArticleParams, noArticleParams)
import AppM (Env)
import Capability.LogMessages (class LogMessages)
import Capability.ManageResource (class ManageResource, getArticles, getTags)
import Capability.Navigate (class Navigate, navigate)
import Capability.Now (class Now)
import Component.HTML.ArticleList (articleList)
import Component.HTML.Footer (footer)
import Component.HTML.Header (header)
import Component.HTML.Utils (css)
import Control.Monad.Reader (class MonadAsk)
import Data.Article (Article)
import Data.Const (Const)
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Data.Route (Route(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..))
import Web.Event.Event (Event, preventDefault)
import Web.UIEvent.MouseEvent.EventTypes (click)

type State =
  { tags :: RemoteData String (Array String)
  , articles :: RemoteData String (Array Article)
  , tab :: Tab
  }

data Tab
  = FeedTab
  | GlobalTab
  | TagTab String

data Query a
  = Init a
  | LoadTags a
  | Navigate Route a
  | ShowTab Tab Event a
  | LoadArticles ArticleParams a

component
  :: forall m
   . MonadAff m
  => MonadAsk Env m
  => Now m
  => LogMessages m
  => ManageResource m
  => Navigate m
  => H.Component HH.HTML Query Unit Void m
component =
  H.lifecycleParentComponent
    { initialState: const { tags: NotAsked, articles: NotAsked, tab: GlobalTab } 
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just $ Init unit
    , finalizer: Nothing
    }

  where 

  eval :: Query ~> H.ParentDSL State Query (Const Void) Void Void m
  eval = case _ of
    Init a -> do
      handle <- H.fork $ eval $ LoadTags a
      eval $ LoadArticles noArticleParams a
    LoadTags a -> do
      H.modify_ _ { tags = Loading}
      tags <- H.lift getTags
      H.modify_ _ { tags = either Failure Success tags }
      pure a
    LoadArticles params a -> do
      H.modify_ _ { articles = Loading }
      articles <- H.lift $ getArticles params
      H.modify_ _ { articles = either Failure Success articles }
      pure a      
    Navigate route a -> do
      navigate route
      pure a
    ShowTab tab ev a -> do
      H.liftEffect $ preventDefault ev
      H.modify_ _{ tab = tab }
      case tab of 
        FeedTab -> pure a -- TODO
        GlobalTab -> eval $ LoadArticles noArticleParams a
        TagTab tag -> eval $ LoadArticles noArticleParams{ tag = Just tag } a
  

  render :: State -> H.ParentHTML Query (Const Void) Void m
  render state@{ tags, articles} =
    HH.div_
    [ header Home Navigate
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
        [ yourFeedTab state.tab
        , globalFeedTab state.tab
        , tagFilterTab state.tab
        ]
      ]
    , articleList state.articles
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
        FeedTab -> " active"
        _ -> ""
  
  globalFeedTab :: forall p. Tab -> H.HTML p Query
  globalFeedTab tab =
    HH.li
    [ css "nav-item" ]
    [ HH.a
      [ css $ "nav-link" <> modifiers
      , HE.handler click $ HE.input $ ShowTab GlobalTab
      ]
      [ HH.text "Global Feed" ]
    ]
    where
      modifiers = case tab of
        GlobalTab -> " active"
        _ -> ""


  tagFilterTab :: forall p i. Tab -> HH.HTML p i
  tagFilterTab = case _ of
    TagTab tag ->
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
    , HE.handler click $ HE.input $ ShowTab (TagTab tag)
    ]
    [ HH.text tag ]
