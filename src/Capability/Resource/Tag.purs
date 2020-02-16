-- | A capability representing the ability to manage tags in our system, which only includes
-- | fetching all tags in the system.
-- |
-- | This capability lets us ignore the mechanics of managing a resource and focus on our
-- | business logic. For now our app implements this capability with a REST API, but we could
-- | easily swap in a database, RPC, local filesystem, or something else without having to touch
-- | any application code besides the application monad, `Conduit.AppM`. In addition, we can test
-- | our business logic by mocking responses in our test monad instead of hitting the server.
-- |
-- | To learn more about why we use capabilities and this architecture, please see the guide:
-- | https://thomashoneyman.com/guides/real-world-halogen/push-effects-to-the-edges/
module Conduit.Capability.Resource.Tag where

import Prelude

import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)

-- | This capability represents the ability to manage tags in our system. Currently we only
-- | need to get all tags.
-- |
-- | We'll handle all the mechanics of making the request, decoding responses, handling errors, and
-- | so on in the implementation.
class Monad m <= ManageTag m where
  getAllTags :: m (Maybe (Array String))

-- | This instance lets us avoid having to use `lift` when we use these functions in a component.
instance manageTagHalogenM :: ManageTag m => ManageTag (HalogenM st act cs msg m) where
  getAllTags = lift getAllTags
