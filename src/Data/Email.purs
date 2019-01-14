module Conduit.Data.Email where

import Prelude

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)

-- We'll newtype `Email` without putting any constraints on its value purely as an 
-- identifier to distinguish it from strings. We'll assume emails are validated on
-- the backend. For that reason, we finally see the `Newtype` class in action, and
-- we don't need to write any toString / parse functions.

newtype Email = Email String

derive instance newtypeEmail :: Newtype Email _
derive instance genericEmail :: Generic Email _
derive instance eqEmail :: Eq Email
derive instance ordEmail :: Ord Email

derive newtype instance encodeJsonEmail :: EncodeJson Email
derive newtype instance decodeJsonEmail :: DecodeJson Email

instance showEmail :: Show Email where
  show = genericShow
