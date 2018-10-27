module Data.Routes where

import Data.User (Username)
import Slug (Slug)

data Route
  = Home
  | Login
  | Logout
  | Register
  | Settings
  | Article (ResourceStatus Slug)
  | Profile ProfileView

-----
-- Routing Types

-- Resources (including articles) can be handled in various states.
data ResourceStatus identifier
  = Create
  | Edit identifier
  | View identifier

-- Profile views can either be the user's articles or articles that
-- a particular user has favorited. Not extensible, as the type is
-- only relevant to user profiles.
data ProfileView
  = User Username
  | Favorites Username
