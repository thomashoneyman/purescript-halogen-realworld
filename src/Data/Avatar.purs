-- | Avatars are strings representing URL locations of images. It's useful to be able to tell at 
-- | a glance that a value isn't just a string -- it's an avatar -- and to prevent accidental
-- | operations on the type that shouldn't be allowed (string functions). In fact, this should be
-- | a largely read-only type in our application, so we'll use the smart constructor pattern to
-- | restrict how values of this type can be created and manipulated.
-- |
-- | For more information on this pattern, please see:
-- | https://thomashoneyman.com/guides/real-world-halogen/design-data-pure-functions/#restricting-the-domain-using-smart-constructors
module Conduit.Data.Avatar 
  ( Avatar -- constructor not exported
  , parse
  , toString
  , toStringWithDefault
  ) where

import Prelude

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))

-- | Avatars are strings representing URL locations of images, and we'll use the `Avatar` type to
-- | distinguish them from usual strings. We'll restrict the type so that not any old string can
-- | be an avatar: they'll have to pass our `parse` validation first. 
-- | 
-- | Not all users have an avatar, which we'll represent with  the correct type: `Maybe Avatar`.
newtype Avatar = Avatar String

derive instance genericAvatar :: Generic Avatar _
derive instance eqAvatar :: Eq Avatar

derive newtype instance encodeJsonAvatar :: EncodeJson Avatar
derive newtype instance decodeJsonAvatar :: DecodeJson Avatar

instance showAvatar :: Show Avatar where
  show = genericShow

-- | While not all users have an avatar, if the `Avatar` type is being used, then we should be 
-- | confident there's actually a URL location inside. We won't validate the URLs (though we could!)
-- | but we *will* verify non-emptiness.
parse :: String -> Maybe Avatar
parse "" = Nothing
parse str = Just (Avatar str)

-- | Since we haven't exported the `Avatar` constructor, we'll include this small function so you
-- | can recover a string from the type.
toString :: Avatar -> String
toString (Avatar str) = str

-- | Avatars are optional, but we don't want to display broken images on our site. This function
-- | provides a fallback avatar for when a user doesn't have one.
toStringWithDefault :: Maybe Avatar -> String
toStringWithDefault (Just av) = toString av
toStringWithDefault Nothing =
  "https://static.productionready.io/images/smiley-cyrus.jpg"
