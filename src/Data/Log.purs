-- | This module provides a data type that can be used to construct log messages
-- | that conform to a particular format we require for our logging service to 
-- | parse for information.

module Data.Log 
  ( LogType(..)
  , message
  , logType
  , Log -- no constructors
  , mkLog
  ) where

import Prelude

import Capability.Now (class Now, nowDateTime)
import Data.Either (either)
import Data.Formatter.DateTime (formatDateTime)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

-- We'll start with a helper data type, LogType, which will help us differentiate 
-- the purpose of a given log.
--
--   Debug: useful for testing, but should not write in production
--   Warn:  write to logging service in production, but not the console
--   Error: write to logging service and the console in production

data LogType = Debug | Warn | Error

derive instance genericLogType :: Generic LogType _
derive instance eqLogType :: Eq LogType
derive instance ordLogType :: Ord LogType

instance showLogType :: Show LogType where
  show = genericShow

-- This type is used to distinguish any possible string from a string that has 
-- specifically been formatted for our logging service to be able to parse. We 
-- won't export its constructors, so it will be difficult to mistakenly write a 
-- bad instance or different implementations in testing vs. our app monad.

-- Not exported
type LogMessage = String

data Log = Log LogType LogMessage

derive instance genericLog :: Generic Log _
derive instance eqLog :: Eq Log

instance showLog :: Show Log where
  show = genericShow

-- We may still need to retrieve the message or log type from the log.

message :: Log -> String
message (Log _ msg) = msg

logType :: Log -> LogType
logType (Log lt _) = lt


-- We want the core implementation of our logging function to be pure. When we 
-- write our instance later on we'll re-use this function, but it can be tested 
-- independently in other contexts like the `Writer` monad or by writing to a 
-- golden test suite.

mkLog
  :: ∀ m
   . Now m
  => LogType      -- | What kind of log is this?
  -> String       -- | What is the input string?
  -> m Log
mkLog lt str = do

  -- Will format "2018-10-25 11:25:29 AM"
  eitherFmt <- formatDateTime "YYYY-DD-MM hh:mm:ss a" <$> nowDateTime

  -- It's possible that we had a typo in our format string, so we'll need
  -- to handle that case. We could use `unsafePartial <<< fromLeft` but I'd
  -- like to be a little more careful by logging a failure message. We can
  -- always test this in a pure context, like the Writer monad, to verify!
  let timestamp = either (const "(Failed to assign time)") identity eitherFmt
  pure $ Log lt $ case lt of
    Debug -> "[DEBUG: " <> timestamp <> "]\n" <> str
    Warn -> "[WARNING: " <> timestamp <> "]\n" <> str
    Error -> "[ERROR: " <> timestamp <> "]\n" <> str