-- | A capability representing the ability to move the user from place to place in the application.
-- | Currently, the production monad implements hash-based routing, but that could easily be replaced
-- | with another method (pushState, for example) without breaking any code outside of `Main`.
-- |
-- | To learn more about why we use capabilities and this architecture, please see the guide:
-- | https://thomashoneyman.com/guides/real-world-halogen/push-effects-to-the-edges/
module Conduit.Capability.Navigate where

import Prelude

import Conduit.Data.Route (Route)
import Control.Monad.Trans.Class (lift)
import Halogen (HalogenM)

-- | This capability represents the ability to move around the application. The `navigate` function
-- | should change the browser location, which will then notify our routing component. The `logout`
-- | function should clear any information associated with the user from the app and browser before
-- | redirecting them to the homepage.
class Monad m <= Navigate m where
  navigate :: Route -> m Unit
  logout :: m Unit

-- | This instance lets us avoid having to use `lift` when we use these functions in a component.
instance navigateHalogenM :: Navigate m => Navigate (HalogenM st act slots msg m) where
  navigate = lift <<< navigate
  logout = lift logout
