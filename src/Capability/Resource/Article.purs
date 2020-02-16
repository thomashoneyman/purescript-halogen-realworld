-- | A capability representing the ability to manage articles in our system, which includes
-- | creation, deletion, updates, favoriting, getting the user feed, and so on.
-- |
-- | This capability lets us ignore the mechanics of managing a resource and focus on our
-- | business logic. For now our app implements this capability with a REST API, but we could
-- | easily swap in a database, RPC, local filesystem, or something else without having to touch
-- | any application code besides the application monad, `Conduit.AppM`. In addition, we can test
-- | our business logic by mocking responses in our test monad instead of hitting the server.
-- |
-- | To learn more about why we use capabilities and this architecture, please see the guide:
-- | https://thomashoneyman.com/guides/real-world-halogen/push-effects-to-the-edges/
module Conduit.Capability.Resource.Article where

import Prelude

import Conduit.Api.Endpoint (ArticleParams, Pagination)
import Conduit.Data.Article (Article, ArticleWithMetadata)
import Conduit.Data.PaginatedArray (PaginatedArray)
import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)
import Slug (Slug)

-- | This capability represents the ability to manage articles in our system. Each function
-- | represents a simple process to read, write, update, delete, favorite, or take some other action
-- | on an article (or articles).
-- |
-- | We'll handle all the mechanics of making the request, decoding responses, handling errors, and
-- | so on in the implementation.
class Monad m <= ManageArticle m where
  getArticle :: Slug -> m (Maybe ArticleWithMetadata)
  getArticles :: ArticleParams -> m (Maybe (PaginatedArray ArticleWithMetadata))
  createArticle :: Article -> m (Maybe ArticleWithMetadata)
  updateArticle :: Slug -> Article -> m (Maybe ArticleWithMetadata)
  deleteArticle :: Slug -> m Unit
  favoriteArticle :: Slug -> m (Maybe ArticleWithMetadata)
  unfavoriteArticle :: Slug -> m (Maybe ArticleWithMetadata)
  getCurrentUserFeed :: Pagination -> m (Maybe (PaginatedArray ArticleWithMetadata))

-- | This instance lets us avoid having to use `lift` when we use these functions in a component.
instance manageArticleHalogenM :: ManageArticle m => ManageArticle (HalogenM st act slots msg m) where
  getArticle = lift <<< getArticle
  getArticles = lift <<< getArticles
  createArticle = lift <<< createArticle
  updateArticle s = lift <<< updateArticle s
  deleteArticle = lift <<< deleteArticle
  favoriteArticle = lift <<< favoriteArticle
  unfavoriteArticle = lift <<< unfavoriteArticle
  getCurrentUserFeed = lift <<< getCurrentUserFeed
