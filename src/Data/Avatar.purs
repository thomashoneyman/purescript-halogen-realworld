module Conduit.Data.Avatar 
  ( Avatar -- constructor not exported
  , parse
  , toString
  , toStringWithDefault
  ) where

-- We'll represent avatars using the smart constructor pattern. Avatars are not 
-- required, but if there is one associated with a user then it cannot be empty. 
-- We'll represent "no avatar" with the `Maybe` type instead of the empty string.

import Prelude

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))

-- We'll represent avatars using the smart constructor pattern. Avatars are not 
-- required, but if there is one associated with a user then it cannot be empty. 
-- We'll represent "no avatar" with the `Maybe` type instead of the empty string.

newtype Avatar = Avatar String

derive instance genericAvatar :: Generic Avatar _
derive instance eqAvatar :: Eq Avatar

derive newtype instance encodeJsonAvatar :: EncodeJson Avatar
derive newtype instance decodeJsonAvatar :: DecodeJson Avatar

instance showAvatar :: Show Avatar where
  show = genericShow

-- For now we'll just verify that if an avatar is meant to exist it is at least 
-- a non-empty string, but as we grow the app we might put more stringent 
-- requirements in place, like requiring a valid URL.

parse :: String -> Maybe Avatar
parse "" = Nothing
parse str = Just (Avatar str)

toString :: Avatar -> String
toString (Avatar str) = str

toStringWithDefault :: Maybe Avatar -> String
toStringWithDefault (Just av) = toString av
toStringWithDefault Nothing =
  "https://static.productionready.io/images/smiley-cyrus.jpg"
