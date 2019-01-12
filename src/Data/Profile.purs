-- | Profiles are an example of an entity (a persistent data type with a unique identity) and are
-- | widely used in the Conduit application, usually as part of a larger type like `Author`.
-- |
-- | We have two versions of the `Profile` type, as with the `Article` type: one with a few core 
-- | fields about the user and another that also contains their email address. It's tedious and 
-- | error-prone to write out multiple variations of a type which all share the same core fields. 
-- | We'll use extensible to solve that problem.
-- |
-- | The backend returns a `following` flag as part of a profile, but this can lead to invalid 
-- | states. See the `Conduit.Data.Author` module for the type-safe solution. 
-- | 
-- | This module also demonstrates creating lenses for record types; we'll use these lenses in
-- | our components to easily drill down from the larger types that contain a `Profile` to a field
-- | we care about in particular.
module Conduit.Data.Profile where

import Conduit.Data.Avatar (Avatar)
import Conduit.Data.Email (Email)
import Conduit.Data.Username (Username)
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Maybe (Maybe)
import Data.Symbol (SProxy(..))

-- | Let's begin by describing the row shared by our main `Profile` type and the extended
-- | `ProfileWithEmail` type. A user profile contains a mandatory username and an optional
-- | biography and avatar. We usually need to know whether the person viewing a profile 
-- | follows that profile, but we capture that information in the `Data.Conduit.Author` type.
type ProfileRep row =
  ( username :: Username
  , bio :: Maybe String
  , image :: Maybe Avatar
  | row
  )

-- | Our main `Profile` type consists only of the fields in this row, whereas our 
-- | `ProfileWithEmail` type extends the row with a new field.
type Profile = { | ProfileRep () }
type ProfileWithEmail = { | ProfileRep ( email :: Email) }

-- | The `Profile` type is usually a part of a larger type, like a `Maybe FollowedAuthor`. Without 
-- | helpers, it  would take several layers of unwrapping before we could finally access the 
-- | `username` field, for example. Sounds terrible!
-- |
-- | But nested structures aren't always a problem. We deal with nested records all the time, for
-- | example, but they're fairly easy to work with because of dot syntax. For example:
-- |
-- | ```purescript
-- | type MyRecord = { x :: { y :: { z :: Int } } }
-- |
-- | getZ :: MyRecord -> Int
-- | getZ rec = rec.x.y.z
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
-- | -- _x, _y, and _z are optics for the "x", "y", and "z" fields in the record 
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
_image :: forall r. Lens' { image :: Maybe Avatar | r } (Maybe Avatar)
_image = prop (SProxy :: SProxy "image")
