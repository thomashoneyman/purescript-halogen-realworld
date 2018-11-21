-- | This module describes the capability to manage external resources in the
-- | system, like users, comments, and articles.

module Capability.ManageResource where

import Prelude

import Api.Endpoint (ArticleParams, Pagination)
import Api.Request (RegisterFields)
import Capability.Authenticate (class Authenticate)
import Data.Article (Article, CreateArticle, UpdateArticle)
import Data.Author (Author)
import Data.Comment (Comment, CommentId, CreateComment)
import Data.Either (Either)
import Data.Profile (Profile)
import Data.Username (Username)
import Slug (Slug)

class Monad m <= ManageResource m where
  register :: RegisterFields -> m (Either String Profile)
  getProfile :: Username -> m (Either String Author)
  getTags :: m (Either String (Array String))
  getComments :: Slug -> m (Either String (Array Comment))
  getArticle :: Slug -> m (Either String Article)
  getArticles :: ArticleParams -> m (Either String (Array Article))

-- Our `Authenticate` class will take care of authenticating our requests. Our `Request`
-- data types will handle constructing those requests. And our concrete implementation
-- in our application monad will simply perform the request and parse the response.

class Authenticate m <= ManageAuthResource m where
  getUser :: m (Either String Profile)
  updateUser :: Profile -> m Unit
  followUser :: Username -> m (Either String Author)
  unfollowUser :: Username -> m (Either String Author)
  createArticle :: CreateArticle -> m (Either String Article)
  updateArticle :: Slug -> UpdateArticle -> m (Either String Article)
  deleteArticle :: Slug -> m Unit
  createComment :: Slug -> CreateComment -> m (Either String Comment)
  deleteComment :: Slug -> CommentId -> m Unit
  favoriteArticle :: Slug -> m (Either String Article)
  unfavoriteArticle :: Slug -> m (Either String Article)
  getFeed :: Pagination -> m (Either String (Array Article))

