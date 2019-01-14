-- A capability that represents managing user resources, which include all variants
-- of the `Profile` data type as well as the `Author` type, as that's just a profile
-- augmented with information about the relationship between the current user and
-- the given profile.

module Conduit.Capability.Resource.User where

import Prelude

import Conduit.Api.Request (AuthFieldsRep, LoginFields, RegisterFields)
import Conduit.Data.Author (Author)
import Conduit.Data.Profile (Profile, ProfileRep, ProfileWithEmail)
import Conduit.Data.Username (Username)
import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)
import Type.Row (type (+))

-- A profile can be updated using any fields from a profile plus any of the auth
-- fields, with a password represented with `Maybe`.
type UpdateProfileFields = { | ProfileRep + AuthFieldsRep Maybe () }

-- Our ManageUser type class will describe what operations we can perform to 
-- acquire, update, and delete users.
class Monad m <= ManageUser m where
  loginUser :: LoginFields -> m (Maybe Profile)
  registerUser :: RegisterFields -> m (Maybe Profile)
  getCurrentUser :: m (Maybe ProfileWithEmail)
  getAuthor :: Username -> m (Maybe Author)
  updateUser :: UpdateProfileFields -> m Unit
  followUser :: Username -> m (Maybe Author)
  unfollowUser :: Username -> m (Maybe Author)

instance manageUserHalogenM :: ManageUser m => ManageUser (HalogenM s f g p o m) where
  loginUser = lift <<< loginUser
  registerUser = lift <<< registerUser
  getCurrentUser = lift getCurrentUser
  getAuthor = lift <<< getAuthor
  updateUser = lift <<< updateUser
  followUser = lift <<< followUser
  unfollowUser = lift <<< unfollowUser
