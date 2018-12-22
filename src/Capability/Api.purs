module Conduit.Capability.Api where

import Prelude

import Affjax (Response, Request)
import Data.Argonaut.Core (Json)

class Monad m <= Api m where
  request :: Request Json -> m (Response Json)