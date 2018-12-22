-- A capability that represents managing user resources, which include all variants
-- of the `Profile` data type as well as the `Author` type, as that's just a profile
-- augmented with information about the relationship between the current user and
-- the given profile.

module Conduit.Capability.Resource.User where

import Prelude

import Conduit.Api.Request (Token)
import Conduit.Data.Author (Author)
import Conduit.Data.Email (Email)
import Conduit.Data.Profile (Profile, ProfileRep, ProfileWithEmail)
import Conduit.Data.Username (Username)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import Halogen (HalogenM, lift)
import Type.Row (type (+))

-- Fields necessary in user management as arguments.

type Unlifted a = a 

type UpdateProfileFields = { | ProfileRep + AuthFieldsRep Maybe () }
type RegisterFields = { | AuthFieldsRep Unlifted (username :: Username) }
type LoginFields = { | AuthFieldsRep Unlifted () }

type AuthFieldsRep f r = ( email :: Email, password :: f String | r )

class Monad m <= ManageUser m where
  getCurrentUser :: m (Maybe ProfileWithEmail)
  getAuthor :: Username -> m (Maybe Author)
  loginUser :: LoginFields -> m (Maybe (Tuple Token Profile))
  registerUser :: RegisterFields -> m (Maybe (Tuple Token Profile))
  updateUser :: UpdateProfileFields -> m Unit
  followUser :: Username -> m (Maybe Author)
  unfollowUser :: Username -> m (Maybe Author)

instance manageUserHalogenM :: ManageUser m => ManageUser (HalogenM s f g p o m) where
  getCurrentUser = lift getCurrentUser
  getAuthor = lift <<< getAuthor
  loginUser = lift <<< loginUser
  registerUser = lift <<< registerUser
  updateUser = lift <<< updateUser
  followUser = lift <<< followUser
  unfollowUser = lift <<< unfollowUser
