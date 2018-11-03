{- This module describes the capability to log messages to some
   output, whether the console, an external service like Rollbar,
   or to files for golden testing. See AppM for the implementation.
-}
module Capability.LogMessages where

import Prelude

import Control.Monad.Trans.Class (lift)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Halogen (HalogenM)

{- We'll start with a helper data type, LogType, which will help
   us differentiate the purpose of a given log.

   Debug: useful for testing, but should not write in production
   Warn:  write to logging service in production, but not the console
   Error: write to logging service and the console in production
-}

data LogType = Debug | Warn | Error

derive instance genericLogType :: Generic LogType _
derive instance eqLogType :: Eq LogType
derive instance ordLogType :: Ord LogType

instance showLogType :: Show LogType where
  show = genericShow

{- The ability to log messages as described with a type class.
   Includes an instance for HalogenM to make this class convenient
   to use in Halogen components.
-}

class Monad m <= LogMessages m where
  log :: LogType -> String -> m Unit

instance logMessagesHalogenM
  :: LogMessages m
  => LogMessages (HalogenM s f g p o m)
  where
  log l = lift <<< log l
