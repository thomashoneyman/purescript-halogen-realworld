module Data.Username where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))

{- Usernames are relevant for the Profile type and the AuthUser
   type, so they are moved out to their own module rather than
   be considered helper data stored with the encompassing type.
-}

newtype Username = Username String

derive instance genericUsername :: Generic Username _
derive instance eqUsername :: Eq Username

derive newtype instance encodeJsonUsername :: EncodeJson Username
derive newtype instance decodeJsonUsername :: DecodeJson Username

instance showUsername :: Show Username where
  show = genericShow

{- We'll use a smart constructor pattern to guarantee that
   usernames in the system are never empty. We may want to
   provide even more validation in the future.
-}

mkUsername :: String -> Maybe Username
mkUsername "" = Nothing
mkUsername str = Just (Username str)

