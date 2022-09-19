-- | A capability representing the ability to manage comments in our system, which includes reading,
-- | writing, and deletion.
-- |
-- | This capability lets us ignore the mechanics of managing a resource and focus on our
-- | business logic. For now our app implements this capability with a REST API, but we could
-- | easily swap in a database, RPC, local filesystem, or something else without having to touch
-- | any application code besides the application monad, `Conduit.AppM`. In addition, we can test
-- | our business logic by mocking responses in our test monad instead of hitting the server.
-- |
-- | To learn more about why we use capabilities and this architecture, please see the guide:
-- | https://thomashoneyman.com/guides/real-world-halogen/push-effects-to-the-edges/
module Conduit.Capability.Resource.Comment where

import Prelude

import Conduit.Data.Comment (Comment, CommentId, CommentWithMetadata)
import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)
import Slug (Slug)

-- | This capability represents the ability to manage comments in our system. Currently we only
-- | need to get all comments, create a comment, or delete a comment, all of which must be
-- | associated with a particular article (uniquely identified by a `Slug`).
-- |
-- | We'll handle all the mechanics of making the request, decoding responses, handling errors, and
-- | so on in the implementation.
class Monad m <= ManageComment m where
  getComments :: Slug -> m (Maybe (Array CommentWithMetadata))
  createComment :: Slug -> Comment -> m (Maybe CommentWithMetadata)
  deleteComment :: Slug -> CommentId -> m Unit

-- | This instance lets us avoid having to use `lift` when we use these functions in a component.
instance manageCommentHalogenM :: ManageComment m => ManageComment (HalogenM st act cs msg m) where
  getComments = lift <<< getComments
  createComment s = lift <<< createComment s
  deleteComment s = lift <<< deleteComment s
