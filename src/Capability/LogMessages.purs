-- | This module describes the capability to log messages to some output, whether 
-- | the console, an external service like Rollbar, or to files for golden testing. 
-- | See AppM for the implementation.

module Capability.LogMessages where

import Prelude

import Capability.Now (class Now)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Log (Log, LogType(..), mkLog)
import Data.Maybe (Maybe(..))
import Halogen (HalogenM)

-- | Log a message to given a particular `LogType` 
log :: forall m. LogMessages m => LogType -> String -> m Unit
log t = logMessage <=< mkLog t

-- | Log a message for debugging purposes  
logDebug :: forall m. LogMessages m => String -> m Unit
logDebug = log Debug 

-- | Log a message as a warning
logWarn :: forall m. LogMessages m => String -> m Unit
logWarn = log Warn 

-- | Log a message as an error
logError :: forall m. LogMessages m => String -> m Unit
logError = log Error 

-- | Hush a monadic action by logging the error
logHush :: forall m a. LogMessages m => LogType -> m (Either String a) -> m (Maybe a)
logHush lt act =
  act >>= case _ of
    Left e -> case lt of
      Debug -> logDebug e *> pure Nothing
      Warn -> logWarn e *> pure Nothing
      Error -> logError e *> pure Nothing
    Right v -> pure $ Just v
   
-- | Hush a monadic action by logging the error in debug mode
debugHush :: forall m a. LogMessages m => m (Either String a) -> m (Maybe a)
debugHush = logHush Debug

-- We will require the `log` type class member to use the `LogMessage` newtype, 
-- which is not exported from this module and can only be constructed using our 
-- pure implementation below. This restricts all instances to a predictable output.

-- In addition, we'll superclass another capability that is required for this one:
-- the ability to get the current time.

class Now m <= LogMessages m where
  logMessage :: Log -> m Unit

instance logMessagesHalogenM :: LogMessages m => LogMessages (HalogenM s f g p o m) where
  logMessage = lift <<< logMessage