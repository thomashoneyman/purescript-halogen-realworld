-- | This module describes the capability to read, write, and clear credentials 
-- | stored about a single user in the application. It is used for local storage 
-- | now, but could be switched to another source later.

module Capability.Authenticate where

import Prelude

import Api.Request (AuthUser, LoginFields)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either)
import Data.Profile (Profile)
import Data.Tuple (Tuple)
import Halogen (HalogenM)

-- The ability to read, write, and clear credentials. Includes an instance for 
-- HalogenM to make this class convenient to use in Halogen components.

class Monad m <= Authenticate m where
  authenticate :: LoginFields -> m (Either String (Tuple AuthUser Profile))
  readAuth :: m (Either String AuthUser)
  writeAuth :: AuthUser -> m Unit
  deleteAuth :: m Unit

instance authenticateHalogenM :: Authenticate m => Authenticate (HalogenM s f g p o m) where
  authenticate = lift <<< authenticate
  readAuth = lift readAuth
  writeAuth = lift <<< writeAuth
  deleteAuth = lift deleteAuth
