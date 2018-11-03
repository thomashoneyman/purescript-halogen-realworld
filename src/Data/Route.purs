module Data.Route where

import Prelude

import Data.Either (note)
import Data.Foldable (oneOf)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.Username (Username, mkUsername)
import Routing.Match (Match, eitherMatch, end, lit, root, str)
import Slug (Slug)
import Slug (parse) as Slug

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
  | Register
  | Settings
  | Article (ResourceStatus Slug)
  | Profile Username
  | Favorites Username
  | NotFound

derive instance genericRoute :: Generic Route _
derive instance eqRoute :: Eq Route

instance showRoute :: Show Route where
  show = genericShow

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

derive instance genericResourceStatus :: Generic (ResourceStatus i) _
derive instance eqResourceStatus :: Eq i => Eq (ResourceStatus i)

instance showResourceStatus :: Show i => Show (ResourceStatus i) where
  show = genericShow

{- We need a way to parse string paths into our routing data type, as
   that's what we'll pull from the browser to manage navigation. We
   are using hash-based navigation, so these should correspond to
   segments after a `/#`.

   TODO: Write tests for route parsing
-}

parseRoute :: Match Route
parseRoute = root *> oneOf
  [ Home <$ end                                  -- | /
  , Login <$ lit "login"                         -- | /login
  , Register <$ lit "register"                   -- | /register
  , Settings <$ lit "settings"                   -- | /settings
  , lit "article" *> map (Article <<< View) slug -- | /article/:slug
  , lit "editor" *> oneOf
      [ Article <<< Edit <$> slug                -- | /editor/:slug
      , pure (Article Create)                    -- | /editor
      ]
  , lit "profile" *> oneOf
      [ Favorites <$> uname <* lit "favorites"   -- | /profile/:username/favorites
      , Profile <$> uname                        -- | /profile/:username
      ]
  , pure NotFound
  ]

{- We use several smart constructors which should restrict which
   routes are valid. Our contract with the backend is that usernames
   must be nonempty and article slugs must be valid, so we will
   parse our paths into those types. If later on we suffer data
   integrity issues and need to be more flexible, then we can adjust
   accordingly.

   This helper function allows us to use our smart constructors as
   part of parsing. We can use it to create helper parsers.
-}

ifParse :: ∀ a. Match (Maybe a) -> Match a
ifParse = eitherMatch <<< map (note unit)

uname :: Match Username
uname = ifParse (mkUsername <$> str)

slug :: Match Slug
slug = ifParse (Slug.parse <$> str)
