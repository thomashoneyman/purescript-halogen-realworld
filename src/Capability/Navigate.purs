{- This module describes the capability to navigate from place to
   place in the application. Currently implemented with hashes, but
   could easily be swapped to another method (like pushState).
-}
module Capability.Navigate where

import Prelude

import Control.Monad.Trans.Class (lift)
import Data.Route (Route)
import Halogen (HalogenM)

{- The ability to navigate around the application. Includes an
   instance for HalogenM to make this class convenient to use in
   Halogen components. A special case is given for logging out,
   as there is no route in our spec meant to handle this case.
-}

class Monad m <= Navigate m where
  navigate :: Route -> m Unit
  logout :: m Unit

instance navigateHalogenM
  :: Navigate m
  => Navigate (HalogenM s f g p o m)
  where
  navigate = lift <<< navigate
  logout = lift logout
