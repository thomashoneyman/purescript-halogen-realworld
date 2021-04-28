-- | We use datetimes all over the place in Conduit to describe when a resource like a comment
-- | or an article was originally created. The `PreciseDateTime` type already exists as part of
-- | a useful library from Awake Security and has utility functions that help translate it to
-- | and from an RFC3339 string, which our API uses to represent datetimes.
-- |
-- | This module demonstrates how to write a codec for a type which doesn't have a basic JSON
-- | representation.
module Conduit.Data.PreciseDateTime where

import Prelude

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Formatter.DateTime (FormatterCommand(..), format)
import Data.List (List, fromFoldable)
import Data.Newtype (unwrap)
import Data.PreciseDateTime (PreciseDateTime)
import Data.PreciseDateTime as PDT
import Data.RFC3339String (RFC3339String(..))

-- | Our API represents datetimes with RFC3339-format strings. We can define a codec which
-- | round-trips from a string representation to a `PreciseDateTime` by describing how to
-- | parse and print that string.
codec :: JsonCodec PreciseDateTime
codec = CA.prismaticCodec "PreciseDateTime" from to CA.string
  where
  from = PDT.fromRFC3339String <<< RFC3339String
  to = unwrap <<< PDT.toRFC3339String

-- | Display a human-readable version of the precise datetime, as described in the Conduit spec
-- |
-- | Example: "Wed Nov 5, 1999"
toDisplayWeekName :: PreciseDateTime -> String
toDisplayWeekName = PDT.toDateTimeLossy >>> format dateFormatter
  where
  dateFormatter :: List FormatterCommand
  dateFormatter = fromFoldable
    [ DayOfWeekNameShort
    , Placeholder " "
    , MonthShort
    , Placeholder " "
    , DayOfMonth
    , Placeholder ", "
    , YearFull
    ]

-- | An alternate way to display a human-readable version of the precise datetime
-- |
-- | Example: "November 5, 1999"
toDisplayMonthDayYear :: PreciseDateTime -> String
toDisplayMonthDayYear = PDT.toDateTimeLossy >>> format dateFormatter
  where
  dateFormatter :: List FormatterCommand
  dateFormatter = fromFoldable
    [ MonthFull
    , Placeholder " "
    , DayOfMonth
    , Placeholder ", "
    , YearFull
    ]
