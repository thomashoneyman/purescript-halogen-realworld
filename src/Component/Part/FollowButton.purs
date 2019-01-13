module Conduit.Component.Part.FollowButton 
  ( followButton
  , follow
  , unfollow
  ) where

import Prelude

import Conduit.Capability.Resource.User (class ManageUser, followUser, unfollowUser)
import Conduit.Component.HTML.Utils (css)
import Conduit.Data.Profile (Relation(..), Profile)
import Conduit.Data.Username (Username)
import Conduit.Data.Username as Username
import Data.Foldable (for_)
import Data.Lens (Traversal', preview, set)
import Data.Maybe (Maybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

-- Our follow button will have behavior that depends on the profile we are interacting
-- with. Since the profile's type already includes information about whether we
-- follow this profile, we can use that to control the behavior of this HTML with
-- the profile type and some embedded queries alone.

followButton 
  :: forall i p
   . H.Action p
  -> H.Action p
  -> Profile 
  -> HH.HTML i (p Unit)
followButton followQuery unfollowQuery profile = case profile.relation of
  Following -> 
    HH.button
      [ css "btn btn-sm action-btn btn-secondary" 
      , HE.onClick $ HE.input_ unfollowQuery
      ]
      [ HH.text $ " Unfollow " <> Username.toString profile.username ]
  NotFollowing -> 
    HH.button
      [ css "btn btn-sm action-btn btn-outline-secondary" 
      , HE.onClick $ HE.input_ followQuery
      ]
      [ HH.i 
        [ css "ion-plus-round"]
        []
      , HH.text $ " Follow " <> Username.toString profile.username
      ]
  You -> HH.text ""


-- In addition to this pure HTML renderer, however, we'd also like to supply the logic 
-- that will work with the queries we've embedded. These two functions will take care
-- of everything we need in `eval` for a component which loads an profile and then
-- performs follow / unfollow actions on it.
--
-- In most cases I don't make assumptions about what is in state nor modify it, but 
-- in this case I'm willing to adopt the convention that somewhere in state is an
-- profile that can be modified.
--
-- The following two functions will handle safely making the request, logging errors,
-- and updating state with the result.

follow  
  :: forall s f g p o m
   . ManageUser m
  => Traversal' s Profile
  -> H.HalogenM s f g p o m Unit
follow _profile = act (eq NotFollowing <<< _.relation) followUser _profile

unfollow  
  :: forall s f g p o m
   . ManageUser m
  => Traversal' s Profile
  -> H.HalogenM s f g p o m Unit
unfollow _profile = act (eq Following <<< _.relation) unfollowUser _profile

-- This will be kept internal.

act  
  :: forall s f g p o m
   . ManageUser m
  => (Profile -> Boolean)
  -> (Username -> m (Maybe Profile))
  -> Traversal' s Profile
  -> H.HalogenM s f g p o m Unit
act cond f _profile = do
  st <- H.get
  for_ (preview _profile st) \profile -> do
    when (cond profile) do
      mbProfile <- H.lift $ f profile.username
      for_ mbProfile $ H.modify_ <<< set _profile
