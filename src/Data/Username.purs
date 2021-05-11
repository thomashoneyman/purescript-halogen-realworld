-- | Usernames are the unique identifier for users in Conduit. You should give meaningful types
-- | to identifiers like usernames so they are not easily mistaken for a usual `String` type and
-- | to prevent inadvertently modifying their contents.
-- |
-- | Not just any `String` will do for a username: we'll require strings to pass validation to
-- | become a `Username`. For now, that validation is simply that the value is non-empty, but we
-- | may impose further rules in the future. This helps us be confident that when we're working
-- | with a `Username` in our code it's actually a valid username, not an arbitrary string.
module Conduit.Data.Username
  ( Username -- constructor not exported
  , parse
  , toString
  , codec
  ) where

import Prelude

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Maybe (Maybe(..))
import Data.Profunctor (dimap)

-- | We'll represent usernames as a newtype wrapper around a string. Newtypes have no performance
-- | overhead, so they're the fastest way to add a documenting type to a primitive type like
-- | `String`.
newtype Username = Username String

derive instance eqUsername :: Eq Username
derive instance ordUsername :: Ord Username

codec :: JsonCodec Username
codec = dimap (\(Username user) -> user) Username CA.string

-- | This function requires a string to pass some validation before being considered a valid
-- | `Username`. For now, we'll just enforce a username is non-empty, but we might introduce more
-- | sophisticated validation later on.
parse :: String -> Maybe Username
parse "" = Nothing
parse str = Just (Username str)

-- | While we don't want to be able to write or manipulate a `Username` without passing validation,
-- | we should still be able to read the string inside. Providing this function makes `Username`
-- | a read-only type.
toString :: Username -> String
toString (Username str) = str
