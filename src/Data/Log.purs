-- | This module defines a type to represent well-formed logs which can be written to an external
-- | logging service.
-- |
-- | Our logs ought to have predictable metadata we can parse and use to gain insight into how our
-- | service is being used and what failures have occurred. To ensure logs are always well-formed,
-- | we'll enforce that only functions from this module can create the `Log` type by using the
-- | smart constructor pattern. To learn more about this pattern, please see:
-- | https://thomashoneyman.com/guides/real-world-halogen/design-data-pure-functions/#restricting-the-domain-using-smart-constructors
-- |
-- | This module is rarely used in the rest of the application. It's a bit too low-level. In our
-- | business logic the critical thing is to report a particular error or message. We shouldn't have
-- | to care about how to format or gather metadata or the mechanics of sending the error to a
-- | particular reporting service.
-- |
-- | The `Conduit.Capability.LogMessages` module describes the higher-level interface to log an
-- | error or message that is used throughout the rest of the application. I'd recommend reading
-- | through that module as well.
module Conduit.Data.Log
  ( LogReason(..)
  , message
  , reason
  , Log -- no constructors exported
  , mkLog
  ) where

import Prelude

import Conduit.Capability.Now (class Now, nowDateTime)
import Data.DateTime (DateTime)
import Data.Either (either)
import Data.Foldable (fold)
import Data.Formatter.DateTime (formatDateTime)

-- | Most of this module describes metadata that can be used to create a predictable logging
-- | format that we can search later on or use to set filters in an external service like Splunk
-- | or Rollbar. Let's start with a piece of metadata to help us differentiate debugging messages,
-- | status information, warnings, and and errors.
data LogReason = Debug | Info | Warn | Error

derive instance eqLogReason :: Eq LogReason
derive instance ordLogReason :: Ord LogReason

-- | We can now write our `Log` type, which contains the metadata about a particular message along
-- | with the correctly-formatted message itself. It may seem redundant to include the metadata in
-- | the type when it has already been used to format the message, but doing so lets us use the
-- | metadata to make decisions about how to send the message. For example, perhaps debugging
-- | messages should never be sent to an external service, and should only be written to the console
-- | in a dev environment (never in production).
-- |
-- | We have not created a newtype instance nor exported the `Log` constructor, so this type cannot
-- | be created except by using functions in this module.
newtype Log = Log
  { reason :: LogReason
  , timestamp :: DateTime
  , message :: String
  }

derive instance eqLog :: Eq Log

-- | We have been careful to prevent creation of the `Log` type outside this module, but we should
-- | still be able to read the fields within. In other words, the type is read-only.
-- |
-- | This helper function retrieves the well-formed message from a `Log`.
message :: Log -> String
message (Log { message: m }) = m

-- | This helper function retrieves the reason a log was produced from a `Log`.
reason :: Log -> LogReason
reason (Log { reason: r }) = r

-- | Let's finally implement the function to create a `Log`. This will be a pure function that
-- | relies on our `Now` capability to grab the current time and write it as an additional piece
-- | of metadata. Our application monad will retrieve the current time effectfully, but we'll
-- | write our tests using a hard-coded time so they can be deterministic.
mkLog :: forall m. Now m => LogReason -> String -> m Log
mkLog logReason inputMessage = do
  now <- nowDateTime

  let
    -- Will produce a header like "{DEBUG: 2018-10-25 11:25:29 AM]\nMessage contents..."
    headerWith start =
      fold [ "[", start, ": ", formatTimestamp now, "]\n", inputMessage ]

    -- Writes the header with the correct log reason
    formattedLog = headerWith case logReason of
      Debug -> "DEBUG"
      Info -> "INFO"
      Warn -> "WARNING"
      Error -> "ERROR"

  pure $ Log { reason: logReason, timestamp: now, message: formattedLog }

  where
  -- Will format "2018-10-25 11:25:29 AM"
  formatTimestamp =
    either (const "(Failed to assign time)") identity
      <<< formatDateTime "YYYY-DD-MM hh:mm:ss a"
