-- | A capability representing the ability to fetch the current time. This is
-- | pure, so we can use fixed times in a test monad for determinism.

module Capability.Now where

import Prelude

import Control.Monad.Trans.Class (lift)
import Data.DateTime (Date, DateTime, Time)
import Data.DateTime.Instant (Instant)
import Halogen (HalogenM)

-- This class may seem trivial -- these are all functions available from the 
-- `purescript-now` package. However, by making this pure, we can use it 
-- in tests deterministically. This is especially useful for snapshot tests.
class Monad m <= Now m where
  now :: m Instant
  nowDate :: m Date
  nowTime :: m Time
  nowDateTime :: m DateTime

instance nowHalogenM :: Now m => Now (HalogenM s f g p o m) where
  now = lift now
  nowDate = lift nowDate
  nowTime = lift nowTime
  nowDateTime = lift nowDateTime