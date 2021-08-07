-- | The follow buttons in Conduit don't have enough encapsulated state or behaviors to be a full
-- | component, but do need to trigger certain actions in a parent component. To avoid writing the
-- | same query handler over and over again, we'll export both the pure HTML function and a default
-- | handle from this module.
module Conduit.Component.Part.FollowButton
  ( followButton
  , follow
  , unfollow
  ) where

import Prelude

import Conduit.Capability.Resource.User (class ManageUser, followUser, unfollowUser)
import Conduit.Component.HTML.Utils (css)
import Conduit.Data.Profile (Author, Relation(..))
import Conduit.Data.Username (Username)
import Conduit.Data.Username as Username
import Data.Foldable (for_)
import Data.Lens (Traversal', preview, set)
import Data.Maybe (Maybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

-- | Our follow button will have behavior that depends on the author we are interacting with.
-- | Since the author's type already includes information about whether we follow this author, we
-- | can use that to control the behavior of this HTML with the author type and some embedded
-- | queries alone.
followButton :: forall props act. act -> act -> Author -> HH.HTML props act
followButton followAct unfollowAct author = case author.relation of
  Following ->
    HH.button
      [ css "btn btn-sm action-btn btn-secondary"
      , HE.onClick \_ -> unfollowAct
      ]
      [ HH.text $ " Unfollow " <> Username.toString author.username ]
  NotFollowing ->
    HH.button
      [ css "btn btn-sm action-btn btn-outline-secondary"
      , HE.onClick \_ -> followAct
      ]
      [ HH.i
          [ css "ion-plus-round" ]
          []
      , HH.text $ " Follow " <> Username.toString author.username
      ]
  You -> HH.text ""

-- | In addition to this pure HTML renderer, however, we'd also like to supply the logic that will
-- | work with the queries we've embedded. These two functions will take care of everything we need
-- | in `eval` for a component which loads an author and then performs follow / unfollow actions
-- | on it.
-- |
-- | In most cases I don't make assumptions about what is in state nor modify it, but in this case
-- | I'm willing to adopt the convention that somewhere in state is an author that can be modified.
-- |
-- | The following two functions will handle safely making the request, logging errors, and updating
-- | state with the result.

follow
  :: forall st act slots msg m
   . ManageUser m
  => Traversal' st Author
  -> H.HalogenM st act slots msg m Unit
follow _author = act (eq NotFollowing <<< _.relation) followUser _author

unfollow
  :: forall st act slots msg m
   . ManageUser m
  => Traversal' st Author
  -> H.HalogenM st act slots msg m Unit
unfollow _author = act (eq Following <<< _.relation) unfollowUser _author

-- | This will be kept internal, as it is only used to implement `follow` and `unfollow`.
act
  :: forall st act slots msg m
   . ManageUser m
  => (Author -> Boolean)
  -> (Username -> m (Maybe Author))
  -> Traversal' st Author
  -> H.HalogenM st act slots msg m Unit
act cond f _author = do
  st <- H.get
  for_ (preview _author st) \author -> do
    when (cond author) do
      mbProfile <- H.lift $ f author.username
      for_ mbProfile $ H.modify_ <<< set _author
