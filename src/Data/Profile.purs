module Conduit.Data.Profile where

import Prelude

import Conduit.Data.Avatar (Avatar)
import Conduit.Data.Email (Email)
import Conduit.Data.Username (Username)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson, (.:))
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either)
import Data.Maybe (Maybe, isJust)
import Data.Symbol (SProxy(..))
import Record as Record

-- Our Profile entity will represent information necessary to render any user 
-- profile in the in the system, including the currently-authenticated one. In
-- addition, different variations of the record are used for receiving extra
-- information or sending extra information to the server.
--
-- Fortunately, with PureScript's nice extensible records, we can easily describe
-- these various states.

type Profile = { | ProfileRep () }

type ProfileWithEmail = { | ProfileRep ( email :: Email) }

type UpdateProfile = { | ProfileRep ( email :: Email, password :: Maybe String ) }

type ProfileRep r =
  ( username :: Username
  , bio :: Maybe String
  , image :: Maybe Avatar
  | r
  )

-- Unfortunately, the object is wrapped in a `User` field, so we have to drill 
-- down a layer  
decodeProfileWithEmail :: Json -> Either String ProfileWithEmail
decodeProfileWithEmail = decodeJson >=> (_ .: "user") >=> decodeJson

-- When encoding a user profile to be updated, we want to ensure that we only
-- send a new password if the user has changed it, indicated by the Maybe status.
-- 
-- The other fields are pre-filled via the API, so it's OK to send to the server
-- even if the user did not set the value.
encodeUpdateProfile :: UpdateProfile -> Json
encodeUpdateProfile rec
  | isJust rec.password = encodeJson rec 
  | otherwise = encodeJson $ Record.delete (SProxy :: SProxy "password") rec