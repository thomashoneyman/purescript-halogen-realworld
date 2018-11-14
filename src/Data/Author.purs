module Data.Author where

import Prelude

import Data.Newtype (class Newtype, unwrap)
import Data.Profile (Profile)
import Data.Username (Username)

-- The Author type is used to represent a user profile with additional context 
-- around whether you, the authenticated user, follow them.

data Author
  = Following FollowedAuthor
  | NotFollowing UnfollowedAuthor
  | You Profile

-- We've written a safe but slightly annoying type. We don't want to have to 
-- deeply pattern match every time we want to pull out an author's username or 
-- profile, for example, so we'll provide some helpers.

username :: Author -> Username
username = _.username <<< unwrap <<< profile

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

-- And another to restrict the domain of functions that are meant only to 
-- operate on *unfollowed* profiles.

newtype UnfollowedAuthor = UnfollowedAuthor Profile

derive instance newtypeUnfollowedAuthor :: Newtype UnfollowedAuthor _
derive instance eqUnfollowedAuthor :: Eq UnfollowedAuthor