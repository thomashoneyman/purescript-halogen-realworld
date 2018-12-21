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

-- This wrapping newtype provides us with the ability to write instances
-- for a type we don't actually own, without receiving errors for orphan
-- instances. We can also define a few helper functions to transform this
-- data type

newtype PreciseDateTime = PreciseDateTime PDT.PreciseDateTime

derive instance newtypePreciseDateTime :: Newtype PreciseDateTime _

instance decodeJsonPreciseDateTime :: DecodeJson PreciseDateTime where
  decodeJson = decodeJson >=> fromString

-- Parse a precise datetime from a string
fromString :: String -> Either String PreciseDateTime
fromString = 
  map PreciseDateTime 
    <<< note "Could not parse RFC339 string" 
    <<< PDT.fromRFC3339String 
    <<< RFC3339String

toDateTime :: PreciseDateTime -> DateTime
toDateTime = unwrap >>> PDT.toDateTimeLossy

toRFC3339String :: PreciseDateTime -> RFC3339String
toRFC3339String = unwrap >>> PDT.toRFC3339String

-- Display a human-readable version of the precise datetime, as described
-- in the Conduit spec

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