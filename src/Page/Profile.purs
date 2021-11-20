-- | User profiles display the articles they have written as well as the articles they have
-- | favorited. It's also the main way users choose to follow one another. Users can view their
-- | own profile.
module Conduit.Page.Profile where

import Prelude

import Conduit.Api.Endpoint (noArticleParams)
import Conduit.Capability.Resource.Article (class ManageArticle, getArticles)
import Conduit.Capability.Resource.User (class ManageUser, getAuthor)
import Conduit.Component.HTML.ArticleList (articleList, renderPagination)
import Conduit.Component.HTML.Footer (footer)
import Conduit.Component.HTML.Header (header)
import Conduit.Component.HTML.Utils (css, maybeElem, safeHref, whenElem)
import Conduit.Component.Part.FavoriteButton (favorite, unfavorite)
import Conduit.Component.Part.FollowButton (follow, followButton, unfollow)
import Conduit.Data.Article (ArticleWithMetadata)
import Conduit.Data.Avatar as Avatar
import Conduit.Data.PaginatedArray (PaginatedArray)
import Conduit.Data.Profile (Author, Profile)
import Conduit.Data.Route (Route(..))
import Conduit.Data.Username (Username)
import Conduit.Data.Username as Username
import Conduit.Store as Store
import Data.Lens (Traversal')
import Data.Lens.Index (ix)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Select (selectEq)
import Network.RemoteData (RemoteData(..), _Success, fromMaybe, toMaybe)
import Type.Proxy (Proxy(..))
import Web.Event.Event (preventDefault)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)

data Action
  = Initialize
  | Receive (Connected (Maybe Profile) Input)
  | LoadArticles
  | LoadFavorites
  | LoadAuthor
  | FollowAuthor
  | UnfollowAuthor
  | FavoriteArticle Int
  | UnfavoriteArticle Int
  | SelectPage Int MouseEvent

type State =
  { articles :: RemoteData String (PaginatedArray ArticleWithMetadata)
  , favorites :: RemoteData String (PaginatedArray ArticleWithMetadata)
  , author :: RemoteData String Author
  , page :: Int
  , currentUser :: Maybe Profile
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

component
  :: forall q o m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => ManageUser m
  => ManageArticle m
  => H.Component q Input o m
component = connect (selectEq _.currentUser) $ H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      , initialize = Just Initialize
      }
  }
  where
  initialState :: Connected (Maybe Profile) Input -> State
  initialState { context: currentUser, input: { username, tab } } =
    { articles: NotAsked
    , favorites: NotAsked
    , author: NotAsked
    , currentUser
    , page: 1
    , tab
    , username
    }

  handleAction :: forall slots. Action -> H.HalogenM State Action slots o m Unit
  handleAction = case _ of
    Initialize -> do
      { tab } <- H.get
      void $ H.fork $ handleAction LoadAuthor
      void $ H.fork $ case tab of
        ArticlesTab -> handleAction LoadArticles
        FavoritesTab -> handleAction LoadFavorites

    Receive { input: { tab, username }, context: currentUser } -> do
      st <- H.get
      when (st.tab /= tab) do
        H.modify_ _ { tab = tab }
        void $ H.fork $ case tab of
          ArticlesTab -> handleAction LoadArticles
          FavoritesTab -> handleAction LoadFavorites
      when (st.username /= username) do
        H.modify_ _ { username = username }
        void $ H.fork $ handleAction Initialize
      when (st.currentUser /= currentUser) do
        H.modify_ _ { currentUser = currentUser }

    LoadArticles -> do
      st <- H.modify _ { articles = Loading }
      let
        params = noArticleParams
          { author = Just st.username
          , offset = if st.page > 1 then Just (st.page * 20) else Nothing
          }
      articles <- getArticles params
      H.modify_ _ { articles = fromMaybe articles }

    LoadFavorites -> do
      st <- H.modify _ { favorites = Loading }
      let
        params = noArticleParams
          { favorited = Just st.username
          , offset = if st.page > 1 then Just (st.page * 20) else Nothing
          }
      favorites <- getArticles params
      H.modify_ _ { favorites = fromMaybe favorites }

    LoadAuthor -> do
      st <- H.modify _ { author = Loading }
      author <- getAuthor st.username
      H.modify_ _ { author = fromMaybe author }

    FollowAuthor ->
      follow _author

    UnfollowAuthor ->
      unfollow _author

    FavoriteArticle index ->
      favorite (_article index)

    UnfavoriteArticle index ->
      unfavorite (_article index)

    SelectPage index event -> do
      H.liftEffect $ preventDefault $ toEvent event
      st <- H.modify _ { page = index }
      void $ H.fork $ handleAction case st.tab of
        FavoritesTab -> LoadFavorites
        ArticlesTab -> LoadArticles

  render :: forall slots. State -> H.ComponentHTML Action slots m
  render state =
    HH.div_
      [ header state.currentUser (Profile state.username)
      , HH.div
          [ css "profile-page" ]
          [ userInfo
          , HH.div
              [ css "container" ]
              [ HH.div
                  [ css "row" ]
                  [ mainView ]
              ]
          ]
      , footer
      ]
    where
    userInfo =
      HH.div
        [ css "user-info" ]
        [ HH.div
            [ css "container" ]
            [ HH.div
                [ css "row" ]
                [ HH.div
                    [ css "col-xs-12 col-md-10 offset-md-1" ]
                    [ HH.img
                        [ css "user-img"
                        , HP.src $ Avatar.toStringWithDefault (_.image =<< toMaybe state.author)
                        ]
                    , HH.h4_
                        [ HH.text $ Username.toString state.username ]
                    , maybeElem (_.bio =<< toMaybe state.author) \str ->
                        HH.p_
                          [ HH.text str ]
                    , maybeElem (toMaybe state.author) (followButton FollowAuthor UnfollowAuthor)
                    ]
                ]
            ]
        ]

    mainView =
      HH.div
        [ css "col-xs-12 col-md-10 offset-md-1" ]
        [ HH.div
            [ css "articles-toggle" ]
            [ HH.ul
                [ css "nav nav-pills outline-active" ]
                [ mkTab ArticlesTab
                , mkTab FavoritesTab
                ]
            ]
        , whenElem (state.tab == ArticlesTab) \_ ->
            HH.div_
              [ articleList FavoriteArticle UnfavoriteArticle state.articles
              , maybeElem (toMaybe state.articles) \paginated ->
                  renderPagination SelectPage state.page paginated
              ]
        , whenElem (state.tab == FavoritesTab) \_ ->
            HH.div_
              [ articleList FavoriteArticle UnfavoriteArticle state.favorites
              , maybeElem (toMaybe state.favorites) \paginated ->
                  renderPagination SelectPage state.page paginated
              ]
        ]

    mkTab thisTab =
      HH.li
        [ css "nav-item" ]
        [ case thisTab of
            ArticlesTab ->
              HH.a
                [ css $ "nav-link" <> guard (state.tab == thisTab) " active"
                , safeHref $ Profile state.username
                ]
                [ HH.text "My Articles" ]
            FavoritesTab ->
              HH.a
                [ css $ "nav-link" <> guard (state.tab == thisTab) " active"
                , safeHref $ Favorites state.username
                ]
                [ HH.text "My Favorites" ]
        ]

_author :: Traversal' State Author
_author = prop (Proxy :: Proxy "author") <<< _Success

_article :: Int -> Traversal' State ArticleWithMetadata
_article i =
  prop (Proxy :: Proxy "articles")
    <<< _Success
    <<< prop (Proxy :: Proxy "body")
    <<< ix i
