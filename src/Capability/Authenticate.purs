-- | This module describes the capability to read, write, and clear credentials 
-- | stored about a single user in the application. It is used for local storage 
-- | now, but could be switched to another source later.

module Capability.Authenticate where

import Prelude

import Control.Monad.Trans.Class (lift)
import Data.AuthUser (AuthUser)
import Data.Either (Either)
import Halogen (HalogenM)

-- The ability to read, write, and clear credentials. Includes an instance for 
-- HalogenM to make this class convenient to use in Halogen components.

class Monad m <= Authenticate m where
  readCredentials :: m (Either String AuthUser)
  writeCredentials :: AuthUser -> m Unit
  deleteCredentials :: m Unit

instance authenticateHalogenM :: Authenticate m => Authenticate (HalogenM s f g p o m) where
  readCredentials = lift readCredentials
  writeCredentials = lift <<< writeCredentials
  deleteCredentials = lift deleteCredentials
