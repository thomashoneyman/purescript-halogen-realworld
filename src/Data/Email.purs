-- | Email addresses are used sparingly in Conduit, but it's still useful to be able to tell 
-- | at a glance that a string is not just a string -- it's an email address.
module Conduit.Data.Email where

import Prelude

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)

-- | This type exists purely as an identifier to distinguish it from a normal `String`, so we'll
-- | create a simple newtype which can be freely wrapped or unwrapped.
newtype Email = Email String

derive instance newtypeEmail :: Newtype Email _
derive instance genericEmail :: Generic Email _
derive instance eqEmail :: Eq Email
derive instance ordEmail :: Ord Email

derive newtype instance encodeJsonEmail :: EncodeJson Email
derive newtype instance decodeJsonEmail :: DecodeJson Email

instance showEmail :: Show Email where
  show = genericShow
