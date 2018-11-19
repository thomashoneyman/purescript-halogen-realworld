-- | This module describes the capability to read, write, and clear credentials 
-- | stored about a single user in the application. It is used for local storage 
-- | now, but could be switched to another source later.

module Capability.Authenticate where

import Prelude

import Api.Request (AuthToken)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either)
import Halogen (HalogenM)

-- Fields necessary to authenticate a user

type AuthFields = 
  { email :: String
  , password :: String 
  }

-- The ability to read, write, and clear credentials. Includes an instance for 
-- HalogenM to make this class convenient to use in Halogen components.

class Monad m <= Authenticate m where
  authenticate :: AuthFields -> m (Either String AuthToken)
  readAuth :: m (Either String AuthToken)
  writeAuth :: AuthToken -> m Unit
  deleteAuth :: m Unit

instance authenticateHalogenM :: Authenticate m => Authenticate (HalogenM s f g p o m) where
  authenticate = lift <<< authenticate
  readAuth = lift readAuth
  writeAuth = lift <<< writeAuth
  deleteAuth = lift deleteAuth
