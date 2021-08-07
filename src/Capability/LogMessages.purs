-- | A capability representing the ability to save status information to some output, which could
-- | be the console, an external service like Splunk, or to the file system for testing purposes.
-- | The implementation can be freely swapped out without changing any application code besides
-- | the application monad, `Conduit.AppM`.
-- |
-- | To learn more about why we use capabilities and this architecture, please see the guide:
-- | https://thomashoneyman.com/guides/real-world-halogen/push-effects-to-the-edges/
module Conduit.Capability.LogMessages where

import Prelude

import Conduit.Capability.Now (class Now)
import Conduit.Data.Log (Log, LogReason(..), mkLog)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Halogen (HalogenM)

-- | We require a strict format for the messages that we log so that we can search this structured
-- | data from our logging service later on. We can enforce this with the type system by using the
-- | `Log` type from `Conduit.Data.Log`. This type can only be constructed using helper functions
-- | in that module, which enforce the correct format. However, we'll leave it up to the implementer
-- | to decide what to do with the log once created.
class Monad m <= LogMessages m where
  logMessage :: Log -> m Unit

-- | This instance lets us avoid having to use `lift` when we use these functions in a component.
instance logMessagesHalogenM :: LogMessages m => LogMessages (HalogenM st act slots msg m) where
  logMessage = lift <<< logMessage

-- | Next, we'll provide a few helper functions to help users easily create and dispatch logs
-- | from anywhere in the application. Each helper composes a couple of small functions together
-- | so that we've got less to remember later on.

-- | Log a message to given a particular `LogType`
log :: forall m. LogMessages m => Now m => LogReason -> String -> m Unit
log reason = logMessage <=< mkLog reason

-- | Log a message for debugging purposes
logDebug :: forall m. LogMessages m => Now m => String -> m Unit
logDebug = log Debug

-- | Log a message to convey non-error information
logInfo :: forall m. LogMessages m => Now m => String -> m Unit
logInfo = log Info

-- | Log a message as a warning
logWarn :: forall m. LogMessages m => Now m => String -> m Unit
logWarn = log Warn

-- | Log a message as an error
logError :: forall m. LogMessages m => Now m => String -> m Unit
logError = log Error

-- | Hush a monadic action by logging the error, leaving it open why the error is being logged
logHush :: forall m a. LogMessages m => Now m => LogReason -> m (Either String a) -> m (Maybe a)
logHush reason action =
  action >>= case _ of
    Left e -> case reason of
      Debug -> logDebug e *> pure Nothing
      Info -> logInfo e *> pure Nothing
      Warn -> logWarn e *> pure Nothing
      Error -> logError e *> pure Nothing
    Right v -> pure $ Just v

-- | Hush a monadic action by logging the error in debug mode
debugHush :: forall m a. LogMessages m => Now m => m (Either String a) -> m (Maybe a)
debugHush = logHush Debug
