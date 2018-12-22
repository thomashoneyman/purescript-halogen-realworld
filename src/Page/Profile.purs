module Conduit.Page.Profile where

import Prelude

import Conduit.Capability.Resource.Article (class ManageArticle, getArticles)
import Conduit.Capability.Resource.User (class ManageUser, getAuthor)
import Conduit.Component.HTML.ArticleList (articleList)
import Conduit.Component.HTML.Footer (footer)
import Conduit.Component.HTML.Header (header)
import Conduit.Component.HTML.Utils (css, maybeElem, safeHref)
import Conduit.Component.Part.FavoriteButton (favorite, unfavorite)
import Conduit.Component.Part.FollowButton (follow, followButton, unfollow)
import Conduit.Data.Article (ArticleWithMetadata)
import Conduit.Data.Author (Author, _You)
import Conduit.Data.Author as Author
import Conduit.Data.Avatar as Avatar
import Conduit.Data.Endpoint (noArticleParams)
import Conduit.Data.Profile (Profile)
import Conduit.Data.Route (Route(..))
import Conduit.Data.Username (Username)
import Conduit.Data.Username as Username
import Control.Parallel (parTraverse_)
import Data.Lens (Traversal', preview)
import Data.Lens.Index (ix)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..), _Success, fromMaybe, isSuccess, toMaybe)

type State =
  { articles :: RemoteData String (Array ArticleWithMetadata)
  , favorites :: RemoteData String (Array ArticleWithMetadata)
  , author :: RemoteData String Author
  , username :: Username
  , tab :: Tab
  }

type Input =
  { username :: Username
  , tab :: Tab
  }

data Tab
  = ArticlesTab
  | FavoritesTab

derive instance eqTab :: Eq Tab

data Query a
  = Initialize a
  | LoadArticles a
  | LoadFavorites a
  | LoadAuthor a
  | FollowAuthor a
  | UnfollowAuthor a
  | FavoriteArticle Int a
  | UnfavoriteArticle Int a
  | ShowTab Tab a

component
  :: forall m
   . MonadAff m
  => ManageUser m
  => ManageArticle m
  => H.Component HH.HTML Query Input Void m
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

  initialState :: Input -> State
  initialState { username, tab } =
    { articles: NotAsked
    , favorites: NotAsked
    , author: NotAsked
    , tab
    , username
    }

  eval :: Query ~> H.ComponentDSL State Query Void m
  eval = case _ of
    Initialize a -> do
      parTraverse_ H.fork [ eval (LoadAuthor a), eval (LoadArticles a) ]
      pure a

    LoadArticles a -> do
      st <- H.modify _ { articles = Loading }
      articles <- getArticles $ noArticleParams { author = Just st.username }
      H.modify_ _ { articles = fromMaybe articles }
      pure a      

    LoadFavorites a -> do
      st <- H.modify _ { favorites = Loading}
      favorites <- getArticles $ noArticleParams { favorited = Just st.username }
      H.modify_ _ { favorites = fromMaybe favorites }
      pure a

    LoadAuthor a -> do
      st <- H.modify _ { author = Loading }
      author <- getAuthor st.username
      H.modify_ _ { author = fromMaybe author }
      pure a
    
    FollowAuthor a -> 
      follow _author $> a

    UnfollowAuthor a -> 
      unfollow _author $> a

    FavoriteArticle index a -> 
      favorite (_article index) $> a

    UnfavoriteArticle index a -> 
      unfavorite (_article index) $> a

    ShowTab thisTab a -> do
      st <- H.get
      when (thisTab /= st.tab) do
        H.modify_ _ { tab = thisTab }
        case thisTab of 
          ArticlesTab -> unless (isSuccess st.articles) do 
            void $ H.fork $ eval $ LoadArticles a
          FavoritesTab -> unless (isSuccess st.favorites) do
            void $ H.fork $ eval $ LoadFavorites a
      pure a
  
  _author :: Traversal' State Author
  _author = prop (SProxy :: SProxy "author") <<< _Success

  _article :: Int -> Traversal' State ArticleWithMetadata
  _article i = prop (SProxy :: SProxy "articles") <<< _Success <<< ix i

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div_
    -- We can use our prisms here to only recover a profile if the author happens 
    -- to be the current user.
    [ header (preview (_You >>> _Success) state.author) Home
    , HH.div
      [ css "profile-page" ]
      [ userInfo state 
      , HH.div
        [ css "container" ] 
        [ HH.div
          [ css "row" ] 
          [ mainView state ]
        ]
      ]
    , footer
    ]
  
  userInfo state =
    HH.div
    [ css "user-info"]
    [ HH.div 
      [ css "container" ]
      [ HH.div 
        [ css "row" ]
        [ HH.div
          [ css "col-xs-12 col-md-10 offset-md-1" ]
          [ HH.img 
            [ css "user-img" 
            , HP.src $ Avatar.toStringWithDefault (_.image =<< profile)
            ]
          , HH.h4_
            [ HH.text $ Username.toString state.username ]
          , maybeElem (_.bio =<< profile) \str ->
              HH.p_
                [ HH.text str ]
          , maybeElem (toMaybe state.author) (followButton FollowAuthor UnfollowAuthor)
          ]
        ]
      ]
    ]
    where
    profile :: Maybe Profile
    profile = pure <<< Author.profile =<< toMaybe state.author


  mainView :: forall i. State -> H.HTML i Query
  mainView state =
    HH.div
    [ css "col-xs-12 col-md-10 offset-md-1" ]
    [ HH.div
      [ css "articles-toggle" ]
      [ HH.ul
        [ css "nav nav-pills outline-active" ]
        [ mkTab state ArticlesTab
        , mkTab state FavoritesTab
        ]
      ]
    , if state.tab == ArticlesTab 
        then articleList FavoriteArticle UnfavoriteArticle state.articles
        else articleList FavoriteArticle UnfavoriteArticle state.favorites
    ]
  
  mkTab :: forall i. State -> Tab -> H.HTML i Query
  mkTab st thisTab =
    HH.li
      [ css "nav-item" ]
      [ HH.a
        [ css $ "nav-link" <> guard (st.tab == thisTab) " active" 
        , HE.onClick $ HE.input_ $ ShowTab thisTab 
        , safeHref $ Profile st.username
        ]
        htmlBody
      ]
    where
    htmlBody = case thisTab of
      ArticlesTab -> [ HH.text "My Articles" ]
      FavoritesTab -> [ HH.text "Favorites" ]
  