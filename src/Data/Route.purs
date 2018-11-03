module Data.Routes where

import Data.Username (Username)
import Slug (Slug)

{- We'll represent routes in our application with a simple sum type.
   As the application grows, you might want to swap this out with
   an extensible sum type with `Variant` and have several sub-
   sections. For our small MVP this type will work just fine and
   will prevent us from trying to send users to non-existent routes.

   These routes are taken from the frontend spec.
-}

data Route
  = Home
  | Login
  | Logout
  | Register
  | Settings
  | Article (ResourceStatus Slug)
  | Profile Username
  | Favorites Username

{- Some resources can be viewed in multiple contexts. For example,
   an article can be created (/editor), edited (/editor/:slug), or
   viewed (/article/:slug). We'll represent those states with a
   separate type.

   Not all constructors require a unique identifier, so we'll only
   use them for editing and viewing.
-}

data ResourceStatus id
  = Create
  | Edit id
  | View id
