-- | Profiles are an example of an entity (a persistent data type with a unique identity) and are
-- | widely used in the Conduit application.
-- |
-- | We have two versions of the `Profile` type, as with the `Article` type: one with a few core
-- | fields about the user and another that also contains their email address. It's tedious and
-- | error-prone to write out multiple variations of a type which all share the same core fields
-- | so we'll use extensible to solve that problem.
-- |
-- | The backend returns a `following` flag as part of a profile, but this can lead to invalid
-- | states. What happens if you're viewing your own profile? There's no concept of following
-- | yourself. We'll create a custom type to rule out invalid relationships like this.
-- |
-- | This module also demonstrates how to create lenses for record types. Optics, which include
-- | lenses, let you work with values within nested structures. We'll use optics to drill down from
-- | large types like a component `State` to a particular field in a profile stored in that state.
module Conduit.Data.Profile where

import Prelude

import Conduit.Data.Avatar (Avatar)
import Conduit.Data.Avatar as Avatar
import Conduit.Data.Email (Email)
import Conduit.Data.Email as Email
import Conduit.Data.Username (Username)
import Conduit.Data.Username as Username
import Data.Codec (mapCodec)
import Data.Codec.Argonaut (JsonCodec, JsonDecodeError)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Compat as CAC
import Data.Codec.Argonaut.Record as CAR
import Data.Either (Either)
import Data.Maybe (Maybe(..))

-- | Let's start by defining a small helper data type before we move on to the larger `Profile` type.
-- |
-- | As far as our JSON is concerned, a user profile includes a username, avatar, biography, and
-- | a unique field: whether the currently-authenticated user is following this profile. This
-- | value is true only when there is a current user and they follow the profile in question.
-- | If there is no current user, or the requested profile belongs to the current user, or the
-- | current user isn't following the requested profile, then the value is `false`.
-- |
-- | Unfortunately, this tricky true/false flag can lead to invalid states in our UI. For example,
-- | user profiles in the application can have a follow button on them. If you don't follow the user
-- | then clicking this button should send a `followUser` request. If you do follow the user, then
-- | it should send an `unfollowUser` request. And if you're viewing your own profile, there should
-- | be no button at all.
-- |
-- | We need a better type: one that represents the *three* possibilities for how a given profile
-- | relates to the (possible) current user. The `Relation` type captures these three cases nicely.
data Relation
  = Following
  | NotFollowing
  | You

derive instance eqRelation :: Eq Relation

-- | Now, let's describe the fields of our main `Profile` type, which should also be shared with
-- | the extended `ProfileWithEmail` type.
-- |
-- | A user profile contains a mandatory username, a biography which is allowed to be empty, an
-- | optional avatar, and a relation to the current user, if there is one. We've already designed
-- | types that nicely capture the semantics of each field, so all we need to do here is assemble
-- | them into a row that can be used to implement `Profile` and `ProfileWithEmail`.
type ProfileRep row =
  ( username :: Username
  , bio :: Maybe String
  , image :: Maybe Avatar
  | row
  )

-- | The `Profile` type consists only of three core fields: the username, biography, and avatar.
type Profile = { | ProfileRep () }

-- | The `ProfileWithEmail` type extends the `Profile` fields with an additional `Email` type.
type ProfileWithEmail = { | ProfileRep (email :: Email) }

-- | The `ProfileWithEmailPassword` type extends the `Profile` fields with additional `email` and `password` fields.
type ProfileWithEmailPassword = { | ProfileRep (email :: Email, password :: Maybe String) }

-- | The `Author` type extends the `Profile` fields with an additional `Relation` type.
type Author = { | ProfileRep (relation :: Relation) }

profileCodec :: JsonCodec Profile
profileCodec =
  CAR.object "Profile"
    { username: Username.codec
    , bio: CAC.maybe CA.string
    , image: CAC.maybe Avatar.codec
    }

profileWithEmailCodec :: JsonCodec ProfileWithEmail
profileWithEmailCodec =
  CAR.object "Profile"
    { username: Username.codec
    , email: Email.codec
    , bio: CAC.maybe CA.string
    , image: CAC.maybe Avatar.codec
    }

profileWithEmailPasswordCodec :: JsonCodec ProfileWithEmailPassword
profileWithEmailPasswordCodec =
  CAR.object "Profile"
    { username: Username.codec
    , email: Email.codec
    , password: CAC.maybe CA.string
    , bio: CAC.maybe CA.string
    , image: CAC.maybe Avatar.codec
    }

authorCodec :: Maybe Username -> JsonCodec Author
authorCodec mbUsername = mapCodec to from codec
  where
  -- We'll stay faithful to our input JSON in the first codec. Then, we'll adjust the result of our
  -- codec using mapCodec so that we can use our Relation type instead of a simple following boolean.
  -- The reason we do that in a separate step is because it depends on having already successfully
  -- parsed the username field.
  codec =
    CAR.object "Author"
      { username: Username.codec
      , bio: CAC.maybe CA.string
      , image: CAC.maybe Avatar.codec
      , following: CA.boolean
      }

  to :: { | ProfileRep (following :: Boolean) } -> Either JsonDecodeError Author
  to { username, bio, image, following } = pure do
    let mkAuthor = { username, bio, image, relation: _ }

    mkAuthor case mbUsername of
      Just user | user == username -> You
      _ | following -> Following
      _ -> NotFollowing

  from :: Author -> { | ProfileRep (following :: Boolean) }
  from { username, bio, image, relation } = do
    let mkAuthorJson = { username, bio, image, following: _ }

    mkAuthorJson case relation of
      Following -> true
      _ -> false
