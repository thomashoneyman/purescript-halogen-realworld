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
import Conduit.Data.Email (Email)
import Conduit.Data.Username (Username)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either)
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))

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

instance decodeJsonFollowStatus :: DecodeJson Relation where
  decodeJson = decodeJson >=> if _ then pure Following else pure NotFollowing 

instance encodeJsonFollowStatus :: EncodeJson Relation where
  encodeJson Following = encodeJson true
  encodeJson _ = encodeJson false

-- | Now, let's describe the fields of our main `Profile` type, which should also be shared with
-- | the extendede `ProfileWithEmail` type.
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

-- | The `Author` type extends the `Profile` fields with an additional `Relation` type.
type Author = { | ProfileRep (relation :: Relation) }

-- | Unfortunately, we can't automatically encode and decode the `Author` type from JSON for 
-- | two reasons.
-- |
-- | First, we already determined that the "following" boolean doesn't adequately cover the three
-- | ways in which a given profile can relate to the person viewing it; to decode this boolean into
-- | our better `Relation` type, we'll need to take the username of the current user (if there is
-- | one) as an argument. Since the `decodeJson :: Json -> Either String a` function doesn't take 
-- | any arguments besides the JSON to decode, we can't write a `DecodeJSON` instance (and 
-- | therefore we certainly can't derive one automatically).
-- |
-- | Second, I've opted to rename the "following" field to the more apt "relation". We can't 
-- | automatically encode and decode when we're changing field names, so we will fall back to a 
-- | manual decoder.
decodeAuthor :: Maybe Username -> Json -> Either String Author
decodeAuthor mbUsername json = do 
  obj <- decodeJson json
  username <- obj .: "username"
  bio <- obj .: "bio"
  image <- obj .: "image"
  relation <- obj .: "following"
  let profile = { username, bio, image, relation }
  pure $ case mbUsername of
    -- If the profile we're decoding from JSON has the same unique identifying username as the one
    -- passed as an argument, then the profile we've decoded is in fact the current user's profile.
    -- So we'll update the `relation` field to the `You` value.
    Just currentUsername | username == currentUsername -> profile { relation = You }
    _ -> profile

-- | The `Profile` type is usually a part of a larger type, like a component's `State`. If you have
-- | a `State` and want to get the username out of a profile stored in that state, you'll have to 
-- | drill down several layers of structure. This is easy to do when the nested structure is made
-- | entirely of records because of dot syntax. We do this all the time! For example:
-- |
-- | ```purescript
-- | type MyRecord = { x :: { y :: { z :: Int } } }
-- |
-- | getZ :: MyRecord -> Int
-- | getZ record = record.x.y.z
-- | ```
-- |
-- | But there are many places where dot syntax (record accessors) fall short. Accessing a value
-- | is convenient, but updating one is more verbose:
-- |
-- | ```purescript
-- | updateZ :: Int -> MyRecord -> MyRecord
-- | updateZ x record = record { x { y { z = x }}}
-- | ```
-- |
-- | Worse, if you need to access or update a field inside a `Maybe` record, you're out of luck.
-- | The special record syntax only helps when you have only records. For example:
-- |
-- | ```purescript
-- | type MyRecordMaybe = { x :: Maybe { y :: Int } }
-- | 
-- | -- we can no longer use just record syntax and have to start mixing in functions that operate
-- | -- on `Maybe` values as well.
-- | getY :: MyRecordMaybe -> Maybe Int
-- | getY record = _.y <$> record.x
-- | ```
-- |
-- | Optics are far more powerful and flexible than dot syntax, but achieve a similar goal: access
-- | to values in nested structures. You can use optics to read a value, like the dot syntax example,
-- | but also to set, modify, and transform values. Even better, they apply widely across types, 
-- | including `Either`, `Maybe`, `Tuple`, your custom types, and more. The best part is that optics 
-- | compose, so drilling into a `Maybe (Either Int (Maybe { x :: Either String (Array Int) }))` 
-- | takes little more effort than our chained record accessors (dots) above.
-- |
-- | Let's see an example! We can define lenses for each of our record fields, compose them 
-- | together, and use `view` to access the `z` field as we did above. But we can also throw som
-- | other types into the mix --  record accessors can't handle that. And we can easily apply
-- | functions to the nested value, too, not just read it. Here are some examples in action:
-- |
-- | ```purescript
-- | type MyVal = { x :: { y :: { z :: Int } } }
-- | type MyValMaybe = { x :: Maybe { y :: Maybe { z :: Int } } }
-- |
-- | -- _x, _y, and _z are lenses (a type of optic) for the "x", "y", and "z" fields in the record.
-- | _x = prop (SProxy :: SProxy "x")
-- | _y = prop (SProxy :: SProxy "y")
-- | _z = prop (SProxy :: SProxy "z")
-- |
-- | getZ :: MyVal -> Int
-- | getZ = view (_x <<< _y <<< _z)
-- |
-- | getMaybeZ :: MyValMaybe -> Maybe Int
-- | getMaybeZ = preview (_x <<< _Just <<< _y <<< _Just <<< _z)
-- |
-- | addFiveToZ :: MyValMaybe -> MyValMaybe
-- | addFiveToZ = over (_x <<< _Just <<< _y <<< _Just <<< _z) (_ + 5)
-- | ```
-- |
-- | Because we tend to work with profiles inside a deeply-nested structure, it will be useful to
-- | be able to leverage optics for them. To do that, we'll define some lenses (a type of optic)
-- | for each of the fields in our profile.

-- | A lens for a username field within a record
_username :: forall r. Lens' { username :: Username | r } Username
_username = prop (SProxy :: SProxy "username")

-- | A lens for a bio field within a record
_bio :: forall r. Lens' { bio :: Maybe String | r } (Maybe String)
_bio = prop (SProxy :: SProxy "bio")

-- | A lens for an image field within a record
_avatar :: forall r. Lens' { avatar :: Maybe Avatar | r } (Maybe Avatar)
_avatar = prop (SProxy :: SProxy "avatar")

-- | A lens for a following field within a record
_relation :: forall r. Lens' { relation :: Relation | r } Relation
_relation = prop (SProxy :: SProxy "relation")