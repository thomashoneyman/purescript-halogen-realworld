-- | Some utilities are useful across any component. We'll maintain them in this catch-all module.
module Conduit.Component.Utils where

import Prelude

import Halogen as H

-- | When a component has no queries or messages, it has no public interface and can be
-- | considered an "opaque" component. The only way for a parent to interact with the
-- | component is by sending input.
type OpaqueSlot slot = forall query. H.Slot query Void slot
