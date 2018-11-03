{- This module describes the capability to log messages to some
   output, whether the console, an external service like Rollbar,
   or to files for golden testing. See AppM for the implementation.
-}

module Capability.LogMessages
  ( LogType(..)
  , class LogMessages
  , log
  , logDebug
  , logWarn
  , logError
  , LogMessage -- constructors not exported
  ) where

import Prelude

import Control.Monad.Trans.Class (lift)
import Data.DateTime (DateTime)
import Data.Either (either)
import Data.Formatter.DateTime (formatDateTime)
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

   We will require the `log` type class member to use the `LogMessage`
   newtype, which is not exported from this module and can only be
   constructed using our pure implementation below. This restricts
   all instances to a predictable output.
-}

class Monad m <= LogMessages m where
  log :: LogMessage -> m Unit
  logDebug :: String -> m Unit
  logWarn :: String -> m Unit
  logError :: String -> m Unit

instance logMessagesHalogenM
  :: LogMessages m
  => LogMessages (HalogenM s f g p o m)
  where
  log = lift <<< log
  logDebug = lift <<< logDebug
  logWarn = lift <<< logWarn
  logError = lift <<< logError


{- This newtype is used to distinguish any possible string from a string
   that has specifically been formatted for our logging service to be able
   to parse. We won't export its constructors, so it will be difficult to
   mistakenly write a bad instance or different implementations in testing
   vs. our app monad.
-}

newtype LogMessage = LogMessage String

derive instance genericLogMessage :: Generic LogMessage _
derive instance eqLogMessage :: Eq LogMessage

instance showLogMessage :: Show LogMessage where
  show = genericShow

{- We want the core implementation of our logging function to be pure.
   When we write our instance later on we'll re-use this function, but
   it can be tested independently in other contexts like the `Writer`
   monad or by writing to a golden test suite.

   TODO: Revise to make a pure function to produce a LogMessage, which
   can then be used with the type class, separate from writing to a file
-}

logMessage
  :: ∀ m
   . Monad m
  => m DateTime             -- | How should we fetch the current time?
  -> (LogMessage -> m Unit) -- | How should we write this message?
  -> LogType                -- | What kind of log is this?
  -> String                 -- | What is the input string?
  -> m Unit
logMessage getTime writeLog logType msg = do

  -- Will format "2018-10-25 11:25:29 AM"
  eitherFmt <- formatDateTime "YYYY-DD-MM hh:mm:ss a" <$> getTime

  -- It's possible that we had a typo in our format string, so we'll need
  -- to handle that case. We could use `unsafePartial <<< fromLeft` but I'd
  -- like to be a little more careful by logging a failure message. We can
  -- always test this in a pure context, like the Writer monad, to verify!
  let timestamp = either (const "(Failed to assign time)") identity eitherFmt
      msg' = LogMessage $ case logType of
        Debug -> "[DEBUG: " <> timestamp <> "]\n" <> msg
        Warn -> "[WARNING: " <> timestamp <> "]\n" <> msg
        Error -> "[ERROR: " <> timestamp <> "]\n" <> msg

  writeLog msg'
