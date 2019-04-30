-- | Articles are stored as markdown strings, which is then rendered as HTMl. They also include
-- | comments so users can read an article and then share what they thought about it. This component
-- | supports viewing and interacting with articles in Conduit.
module Conduit.Page.ViewArticle where

import Prelude

import Conduit.Capability.Navigate (class Navigate, navigate)
import Conduit.Capability.Resource.Article (class ManageArticle, deleteArticle, getArticle)
import Conduit.Capability.Resource.Comment (class ManageComment, createComment, deleteComment, getComments)
import Conduit.Capability.Resource.User (class ManageUser)
import Conduit.Component.HTML.Footer (footer)
import Conduit.Component.HTML.Header (header)
import Conduit.Component.HTML.Utils (css, maybeElem, safeHref, whenElem)
import Conduit.Component.Part.FavoriteButton (ButtonSize(..), favorite, favoriteButton, unfavorite)
import Conduit.Component.Part.FollowButton (follow, followButton, unfollow)
import Conduit.Component.RawHTML as RawHTML
import Conduit.Data.Article (ArticleWithMetadata)
import Conduit.Data.Avatar as Avatar
import Conduit.Data.Comment (Comment, CommentId)
import Conduit.Data.PreciseDateTime as PDT
import Conduit.Data.Profile (Profile, Relation(..), Author)
import Conduit.Data.Route (Route(..))
import Conduit.Data.Username as Username
import Conduit.Env (UserEnv)
import Control.Monad.Reader (class MonadAsk, asks)
import Control.Parallel (parTraverse_)
import Data.Const (Const)
import Data.Foldable (for_)
import Data.Lens (Traversal', preview)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Ref as Ref
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..), _Success, fromMaybe)
import Slug (Slug)

data Action
  = Initialize
  | GetArticle
  | GetComments
  | AddComment
  | UpdateCommentText String
  | FollowAuthor
  | UnfollowAuthor
  | FavoriteArticle
  | UnfavoriteArticle
  | DeleteArticle
  | DeleteComment CommentId

type State =
  { article :: RemoteData String ArticleWithMetadata
  , comments :: RemoteData String (Array Comment)
  , myComment :: String
  , slug :: Slug
  , currentUser :: Maybe Profile
  }

type Input =
  { slug :: Slug
  }

type ChildSlots = 
  ( rawHtml :: H.Slot (Const Void) Void Unit )

component
  :: forall m r
   . MonadAff m
  => ManageArticle m
  => ManageComment m
  => ManageUser m
  => MonadAsk { userEnv :: UserEnv | r } m
  => Navigate m
  => H.Component HH.HTML (Const Void) Input Void m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction 
      , initialize = Just Initialize
      }
  }
  where 
  initialState :: Input -> State
  initialState { slug } = 
    { article: NotAsked
    , comments: NotAsked
    , myComment: ""
    , currentUser: Nothing 
    , slug
    }

  handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
  handleAction = case _ of
    Initialize -> do
      parTraverse_ H.fork [ handleAction GetArticle, handleAction GetComments ]
      mbProfile <- H.liftEffect <<< Ref.read =<< asks _.userEnv.currentUser
      H.modify_ _ { currentUser = mbProfile } 

    GetArticle -> do
      st <- H.modify _ { article = Loading }
      article <- getArticle st.slug
      H.modify_ _ { article = fromMaybe article }

    GetComments -> do
      st <- H.modify _ { comments = Loading }
      comments <- getComments st.slug
      H.modify_ _ { comments = fromMaybe comments }
    
    AddComment -> do
      st <- H.get
      when (st.myComment /= "") do
        for_ (preview _Success st.article) \article -> do
          void $ createComment article.slug st.myComment
          comments <- getComments st.slug
          H.modify_ _ { comments = fromMaybe comments, myComment = "" }

    UpdateCommentText str ->
      H.modify_ _ { myComment = str }

    FollowAuthor -> 
      follow _author

    UnfollowAuthor -> 
      unfollow _author

    FavoriteArticle -> 
      favorite _article

    UnfavoriteArticle -> 
      unfavorite _article

    DeleteArticle -> do
      st <- H.get
      for_ (preview _Success st.article) (deleteArticle <<< _.slug) 
      navigate Home
    
    DeleteComment commentId -> do
      st <- H.get
      deleteComment st.slug commentId
      comments <- getComments st.slug
      H.modify_ _ { comments = fromMaybe comments }
  
  _author :: Traversal' State Author
  _author = _article <<< prop (SProxy :: SProxy "author")

  _article :: Traversal' State ArticleWithMetadata
  _article = prop (SProxy :: SProxy "article") <<< _Success

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state =
    HH.div
      [ css "article-page" ]
      [ header state.currentUser (ViewArticle state.slug)
      , maybeElem mbArticle banner
      , maybeElem mbArticle content
      , footer
      ]
    where
    mbArticle = preview _Success state.article
    markdown = Maybe.fromMaybe "Failed to load article!" (_.body <$> mbArticle)

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

    content article =
      HH.div
        [ css "container page" ]
        [ HH.div
            [ css "col-xs-12" ]
            [ HH.slot (SProxy :: _ "rawHtml") unit RawHTML.component { markdown } absurd 
            , HH.ul  
                [ css "tag-list" ]
                (renderTag <$> article.tagList)
            , HH.hr_
            , HH.div
                [ css "article-actions" ]
                [ articleMeta article ]
            , HH.div
                [ css "row" ]
                [ HH.div
                    [ css "col-xs-12 col-md-8 offset-md-2" ]
                    ( append 
                      [ maybeElem state.currentUser \profile ->
                          HH.form 
                            [ css "card comment-form"
                            , HE.onSubmit \_ -> Just AddComment 
                            ]
                            [ HH.div
                                [ css "card-block" ]
                                [ HH.textarea 
                                    [ css "form-control" 
                                    , HP.placeholder "Write a comment..." 
                                    , HP.rows 3
                                    , HE.onValueInput $ Just <<< UpdateCommentText
                                    ]
                                ]
                            , HH.div
                                [ css "card-footer" ]
                                [ HH.img 
                                    [ css "comment-author-img" 
                                    , HP.src $ Avatar.toStringWithDefault profile.image
                                    ]
                                , HH.button
                                    [ css "btn btn-sm btn-primary" 
                                    , HP.type_ HP.ButtonSubmit 
                                    ]
                                    [ HH.text "Post Comment" ]
                                ]
                            ]
                      ]
                      case preview _Success state.comments of 
                        Nothing -> [ HH.text "" ]
                        Just arr -> viewComment <$> arr
                    )
                ]
            ]
        ]
      where
      renderTag str = 
        HH.li 
          [ css "tag-default tag-pill tag-outline" ] 
          [ HH.text str ]
    
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
                [ HH.text $ PDT.toDisplayMonthDayYear article.createdAt ]
            ]
        , case state.currentUser of
            Just profile | profile.username == username ->
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
                    , HE.onClick \_ -> Just DeleteArticle
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
      username = article.author.username
      avatar = article.author.image

    viewComment comment =
      HH.div
        [ css "card" ]
        [ HH.div 
            [ css "card-block" ]
            [ HH.p 
                [ css "card-text" ]
                [ HH.text comment.body ]
            ]
        , HH.div
          [ css "card-footer" ]
          [ HH.a
              [ css "comment-author" 
              , safeHref $ Profile comment.author.username
              ]
              [ HH.img 
                  [ css "comment-author-img" 
                  , HP.src $ Avatar.toStringWithDefault comment.author.image
                  ]
              ]
          , HH.text " "
          , HH.a
              [ css "comment-author" 
              , safeHref $ Profile comment.author.username
              ]
              [ HH.text $ Username.toString comment.author.username ]
          , HH.text " "
          , HH.span
              [ css "date-posted" ]
              [ HH.text $ PDT.toDisplayMonthDayYear comment.createdAt ]
          , whenElem (comment.author.relation == You) \_ ->
              HH.span
                [ css "mod-options" ]
                [ HH.i 
                    [ css "ion-trash-a" 
                    , HE.onClick \_ -> Just $ DeleteComment comment.id
                    ]
                    []
                ]
          ]
        ]
