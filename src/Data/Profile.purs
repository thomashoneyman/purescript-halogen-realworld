module Conduit.Data.Profile where

import Conduit.Data.Avatar (Avatar)
import Conduit.Data.Username (Username)
import Data.Maybe (Maybe)

-- Our Profile entity will represent information necessary to render any user 
-- profile in the in the system, including the currently-authenticated one. We'll 
-- use a newtype rather than a type synonym over a record so that we can write 
-- JSON instances for the type.

type Profile =
  { username :: Username
  , bio :: Maybe String
  , image :: Maybe Avatar
  }
