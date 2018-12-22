module Conduit.Capability.Resource.Tag where

import Prelude

import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)

class Monad m <= ManageTag m where
  getAllTags :: m (Maybe (Array String))

instance manageTagHalogenM :: ManageTag m => ManageTag (HalogenM s f g p o m) where
  getAllTags = lift getAllTags

