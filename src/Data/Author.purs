module Data.Author where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Argonaut.Encode (class EncodeJson)
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Profile (Profile)
import Data.Username (Username)

-- The Author type is used to represent a user profile with additional context 
-- around whether you, the authenticated user, follow them.

data Author
  = Following FollowedAuthor
  | NotFollowing UnfollowedAuthor
  | You Profile

-- We'll write a manual decoder instead of a decode instance so that we can test
-- if the current user is the same as the one received in the object.

decodeAuthor :: Maybe Username -> Json -> Either String Author
decodeAuthor Nothing json = do
  prof <- decodeJson json
  pure $ NotFollowing $ UnfollowedAuthor prof
decodeAuthor (Just u) json = do
  prof <- decodeJson json
  if prof.username == u
    then pure $ You prof
    else do
      following <- (_ .: "following") =<< decodeJson json
      if following
        then pure $ Following $ FollowedAuthor prof
        else pure $ NotFollowing $ UnfollowedAuthor prof
  


-- We've written a safe but slightly annoying type. We don't want to have to 
-- deeply pattern match every time we want to pull out an author's username or 
-- profile, for example, so we'll provide some helpers.

username :: Author -> Username
username = _.username <<< profile

profile :: Author -> Profile
profile (Following (FollowedAuthor p)) = p
profile (NotFollowing (UnfollowedAuthor p)) = p
profile (You p) = p

-- We'll use a newtype to restrict the domain of functions that are meant to 
-- only operate on *followed* profiles. This lets us write those functions 
-- without worrying about the other cases.

newtype FollowedAuthor = FollowedAuthor Profile

derive instance newtypeFollowedAuthor :: Newtype FollowedAuthor _
derive instance eqFollowedAuthor :: Eq FollowedAuthor

derive newtype instance decodeJsonFollowedAuthor :: DecodeJson FollowedAuthor
derive newtype instance encodeJsonFollowedAuthor :: EncodeJson FollowedAuthor

-- And another to restrict the domain of functions that are meant only to 
-- operate on *unfollowed* profiles.

newtype UnfollowedAuthor = UnfollowedAuthor Profile

derive instance newtypeUnfollowedAuthor :: Newtype UnfollowedAuthor _
derive instance eqUnfollowedAuthor :: Eq UnfollowedAuthor

derive newtype instance decodeJsonUnfollowedAuthor :: DecodeJson UnfollowedAuthor
derive newtype instance encodeJsonUnfollowedAuthor :: EncodeJson UnfollowedAuthor