module Conduit.Data.Profile where


import Conduit.Data.Avatar (Avatar)
import Conduit.Data.Email (Email)
import Conduit.Data.Username (Username)
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Maybe (Maybe)
import Data.Symbol (SProxy(..))

-- Our Profile entity will represent information necessary to render any user 
-- profile in the in the system, including the currently-authenticated one. In
-- addition, different variations of the record are used for receiving extra
-- information or sending extra information to the server.
--
-- Fortunately, with PureScript's nice extensible records, we can easily describe
-- these various states.

type Profile = { | ProfileRep () }
type ProfileWithEmail = { | ProfileRep ( email :: Email) }

type ProfileRep r =
  ( username :: Username
  , bio :: Maybe String
  , image :: Maybe Avatar
  | r
  )

-- We work with two fields in particular all the time, so for convenience, we can
-- write lenses that allow us to drill down into the structure. Lenses for records
-- are essentially identical with the dot syntax you're used to, but can be composed
-- with lenses for other kinds of data like our `Author` data type, and can be used
-- to set or modify values (this is what powers Record.set and Record.modify under
-- the hood).

_username :: forall r. Lens' { username :: Username | r } Username
_username = prop (SProxy :: SProxy "username")

_bio :: forall r. Lens' { bio :: Maybe String | r } (Maybe String)
_bio = prop (SProxy :: SProxy "bio")

_image :: forall r. Lens' { image :: Maybe Avatar | r } (Maybe Avatar)
_image = prop (SProxy :: SProxy "image")
