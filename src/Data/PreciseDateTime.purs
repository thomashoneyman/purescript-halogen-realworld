-- | We use datetimes all over the place in Conduit to describe when a resource like a comment
-- | or an article was originally created. The `PreciseDateTime` type already exists as part of
-- | a useful library from Awake Security, but it doesn't have any `DecodeJson` or `EncodeJson`
-- | instances. After all, there are many ways you could represent a datetime as a string!
-- |
-- | I want to be able to generically encode and decode records that contain precise datetimes,
-- | though. I can't write type class instances for a type I don't own, so I can't write them for 
-- | the `Data.PreciseDateTime` type directly.
-- |
-- | Instead, this module demonstrates how to use a tiny wrapping type to write our own type class
-- | instances for types from external modules.
module Conduit.Data.PreciseDateTime where

import Prelude

import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.DateTime (DateTime)
import Data.Either (Either, note)
import Data.Formatter.DateTime (FormatterCommand(..), format)
import Data.List (List, fromFoldable)
import Data.Newtype (class Newtype, unwrap)
import Data.PreciseDateTime as PDT
import Data.RFC3339String (RFC3339String(..))

-- | Newtypes have no runtime representation, so this small wrapping type lets us write new 
-- | instances for the `Data.PreciseDateTime` type without incurring costs. Since we can always
-- | remove this wrapper, we still get to access all functions and type class instances that
-- | operate on the original type, too.
newtype PreciseDateTime = PreciseDateTime PDT.PreciseDateTime

derive instance newtypePreciseDateTime :: Newtype PreciseDateTime _

-- | For example, we can now define a JSON decoder for the type by expecting one particular 
-- | string representation.
instance decodeJsonPreciseDateTime :: DecodeJson PreciseDateTime where
  decodeJson = fromString <=< decodeJson

-- | Try to parse a `PreciseDateTime` from a string.
fromString :: String -> Either String PreciseDateTime
fromString = 
  map PreciseDateTime 
    <<< note "Could not parse RFC339 string" 
    <<< PDT.fromRFC3339String 
    <<< RFC3339String

-- | Convert a precise datetime into a less-precise JS-based datetime
toDateTime :: PreciseDateTime -> DateTime
toDateTime = unwrap >>> PDT.toDateTimeLossy

-- | Convert a precise datetime into a string representation according to RFC3339
toRFC3339String :: PreciseDateTime -> RFC3339String
toRFC3339String = unwrap >>> PDT.toRFC3339String

-- | Display a human-readable version of the precise datetime, as described in the Conduit spec
-- |
-- | Example: "Wed Nov 5, 1999"
toDisplayWeekName :: PreciseDateTime -> String
toDisplayWeekName = toDateTime >>> format dateFormatter
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
toDisplayMonthDayYear = toDateTime >>> format dateFormatter
  where
  dateFormatter :: List FormatterCommand
  dateFormatter = fromFoldable
    [ MonthFull
    , Placeholder " "
    , DayOfMonth
    , Placeholder ", "
    , YearFull
    ]