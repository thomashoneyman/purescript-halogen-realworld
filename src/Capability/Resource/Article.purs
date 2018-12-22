module Conduit.Capability.Resource.Article where

import Prelude

import Conduit.Data.Article (Article, ArticleWithMetadata)
import Conduit.Data.Endpoint (ArticleParams, Pagination)
import Conduit.Data.PaginatedArray (PaginatedArray)
import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)
import Slug (Slug)

class Monad m <= ManageArticle m where
  getArticle :: Slug -> m (Maybe ArticleWithMetadata)
  getArticles :: ArticleParams -> m (Maybe (PaginatedArray ArticleWithMetadata))
  createArticle :: Article -> m (Maybe ArticleWithMetadata)
  updateArticle :: Slug -> Article -> m (Maybe ArticleWithMetadata)
  deleteArticle :: Slug -> m Unit
  favoriteArticle :: Slug -> m (Maybe ArticleWithMetadata)
  unfavoriteArticle :: Slug -> m (Maybe ArticleWithMetadata)
  getCurrentUserFeed :: Pagination -> m (Maybe (PaginatedArray ArticleWithMetadata))

instance manageArticleHalogenM :: ManageArticle m => ManageArticle (HalogenM s f g p o m) where
  getArticle = lift <<< getArticle
  getArticles = lift <<< getArticles
  createArticle = lift <<< createArticle
  updateArticle s = lift <<< updateArticle s
  deleteArticle = lift <<< deleteArticle
  favoriteArticle = lift <<< favoriteArticle
  unfavoriteArticle = lift <<< unfavoriteArticle
  getCurrentUserFeed = lift <<< getCurrentUserFeed

