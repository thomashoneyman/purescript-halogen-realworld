module Conduit.Capability.Resource.Comment where

import Prelude

import Conduit.Data.Comment (Comment, CommentId, CreateComment)
import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)
import Slug (Slug)

class Monad m <= ManageComment m where
  getComments :: Slug -> m (Maybe (Array Comment))
  createComment :: Slug -> CreateComment -> m Unit
  deleteComment :: Slug -> CommentId -> m Unit

instance manageCommentHalogenM :: ManageComment m => ManageComment (HalogenM s f g p o m) where
  getComments = lift <<< getComments
  createComment s = lift <<< createComment s
  deleteComment s = lift <<< deleteComment s

