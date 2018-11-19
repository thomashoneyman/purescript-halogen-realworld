module Data.Route where

import Prelude hiding ((/))

import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Username (Username)
import Data.Username as Username
import Routing.Duplex (RouteDuplex', as, root, segment)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))
import Slug (Slug)
import Slug as Slug

-- We'll represent routes in our application with a simple sum type. As the 
-- application grows, you might want to swap this out with an extensible sum 
-- type with `Variant` and have several sub-sections. For our small MVP this 
-- type will work just fine and will prevent us from trying to send users to 
-- non-existent routes. These routes are taken from the frontend spec.

data Route
  = Home
  | Login
  | Register
  | Settings
  | Editor
  | EditArticle Slug
  | ViewArticle Slug
  | Profile Username
  | Favorites Username

derive instance genericRoute :: Generic Route _
derive instance eqRoute :: Eq Route
derive instance ordRoute :: Ord Route

instance showRoute :: Show Route where
  show = genericShow

-- Next, we'll define a bidirectional codec for our route parsing. Our single
-- codec will handle both parsing browser locations and serializing our data type
-- to a browser location. We'll skip the boilerplate of separate encoding and
-- decoding functions, and we'll ensure our parsing and printing is always in sync.

-- Our codec will cause a compile-time error if we fail to handle any of our 
-- route cases.

routeCodec :: RouteDuplex' Route
routeCodec = root $ sum
  { "Home": noArgs
  , "Login": "login" / noArgs
  , "Register": "register" / noArgs
  , "Settings": "settings" / noArgs
  , "Editor": "editor" / noArgs
  , "EditArticle": "editor" / slug segment
  , "ViewArticle": "article" / slug segment
  , "Profile": "profile" / uname segment 
  , "Favorites": "profile" / uname segment / "favorites"
  }

-- We can define our own combinators

slug :: RouteDuplex' String -> RouteDuplex' Slug
slug = as Slug.toString (Slug.parse >>> note "Bad slug")

uname :: RouteDuplex' String -> RouteDuplex' Username
uname = as Username.toString (Username.parse >>> note "Bad username")