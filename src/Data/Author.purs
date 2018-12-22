module Conduit.Data.Author where

import Prelude

import Conduit.Data.Profile (Profile)
import Conduit.Data.Username (Username)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Argonaut.Encode (class EncodeJson)
import Data.Either (Either)
import Data.Lens (Prism', prism')
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)

-- The Author type is used to represent a user profile with additional context 
-- around whether you, the authenticated user, follow them.

data Author
  = Following FollowedAuthor
  | NotFollowing UnfollowedAuthor
  | You Profile

-- This is a reasonably nested type, so we'll likely want to easily recover parts
-- of the type like a profile, username, or avatar. We can do that by defining a
-- few prisms (an optic for values that may or may not exist).
--
-- For example, to retrieve a profile of a followed user, we could write:
--
-- getFollowedProfile :: Author -> Maybe Profile
-- getFollowedProfile = preview (_Newtype >>> _Following)

_Following :: Prism' Author FollowedAuthor
_Following  = prism' Following case _ of
  Following author -> Just author
  _ -> Nothing

_NotFollowing :: Prism' Author UnfollowedAuthor
_NotFollowing  = prism' NotFollowing case _ of
  NotFollowing author -> Just author
  _ -> Nothing

_You :: Prism' Author Profile
_You  = prism' You case _ of
  You prof -> Just prof
  _ -> Nothing

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

-- We can use our prisms and lenses to drill down any particular pathway in the 
-- data type, but sometimes we want to just get a shared piece of information 
-- no matter where in the structure we have to get it from. 

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