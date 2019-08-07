module Conduit.Data.Utils where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Struct.Tolerant ((.::))
import Data.Argonaut.Decode.Struct.Tolerant as Tolerant
import Data.Either (Either)

-- | We can decode records and primitive types automatically, and we've defined custom decoders for
-- | our custom data types. However, our API frequently returns those data structures wrapped in
-- | a larger object with a single field like "user", "profile", or "article". This utility allows
-- | us to decode a JSON object with a particular key, and then decode the contents.
-- |
-- | For example, consider this JSON object containing a single field, "user", which itself contains
-- | a JSON object representing a user profile:
-- |
-- | ```json
-- | { "user": { "username": ... } }
-- | ```
-- |
-- | We can make our `Profile` decoder compatible with this new JSON using our `decodeAt` helper:
-- |
-- | ```purescript
-- | decodeProfile :: Json -> Either String Profile
-- | decodeProfile = decodeAt "user"
-- | ```

decodeAt :: forall a. Tolerant.DecodeJson a => String -> Json -> Either String a
decodeAt key = (_ .:: key) <=< decodeJson
