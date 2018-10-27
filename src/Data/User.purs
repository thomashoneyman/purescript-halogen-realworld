-- | Data types related to users

module Data.User where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)

newtype Username = Username String
derive instance genericUsername :: Generic Username _
derive instance newtypeUsername :: Newtype Username _
derive instance eqUsername :: Eq Username

derive newtype instance encodeJsonUsername :: EncodeJson Username
derive newtype instance decodeJsonUsername :: DecodeJson Username

instance showUsername :: Show Username where
  show = genericShow
