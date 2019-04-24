-- | Some utilities are useful across any component. We'll maintain them in this catch-all module.
module Conduit.Component.Utils where

import Prelude

import Conduit.Capability.Navigate (class Navigate)
import Conduit.Data.Profile (Profile)
import Control.Monad.Reader (class MonadAsk, ask)
import Control.Monad.Rec.Class (forever)
import Data.Const (Const)
import Data.Maybe (Maybe)
import Effect.Aff (error, forkAff, killFiber)
import Effect.Aff.Bus as Bus
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Halogen as H
import Halogen.Query.EventSource as ES

-- | When a component has no queries or messages, it has no public interface and can be
-- | considered an "opaque" component. The only way for a parent to interact with the component
-- | is by sending input.
type OpaqueSlot = H.Slot (Const Void) Void

-- | Sometimes it's useful for a component to subscribe to a stream of incoming information. 
-- | Halogen provides 'event sources' for this purpose. For example, you can send messages to
-- | subscribers when key events occur on the global window, so multiple components subscribe to
-- | and are notified about these events.
-- |
-- | At other times it's useful to subscribe to non-DOM events. The most common of these is when 
-- | you have a global state with a piece of mutable data and multiple components need to stay in
-- | sync about the current value of that data. Each time the data is changed, you can broadcast
-- | the change to subscribed components so they always have the correct information.
-- |
-- | In our case, we'll use this to subscribe components to updates about the value of the current
-- | user in global state.
-- |
-- | This helper function helps create an event source from a many-to-many bus. For example:
-- | 
-- | ```purescript
-- | handleAction = case _ of 
-- |   Initialize -> do
-- |     { bus } <- ask
-- |     subscribeWithAction HandleBus bus
-- |   
-- |   HandleBus busMessage -> do
-- |     ...
-- | ```
subscribeWithAction
  :: forall busMsg st act slots msg m r
   . MonadAff m 
  => (busMsg -> act) 
  -> Bus.BusR' r busMsg 
  -> H.HalogenM st act slots msg m Unit
subscribeWithAction handler bus =
  void $ H.subscribe $ ES.affEventSource \emitter -> do
    fiber <- forkAff $ forever $ ES.emit emitter <<< handler =<< Bus.read bus
    pure (ES.Finalizer (killFiber (error "Event source closed") fiber))

-- | Some components need to retrieve the current value of the logged-in user from state and would
-- | also like to subscribe to further updates at the same time. This helper function bundles that
-- | behavior so you can both retrieve the value and subscribe to further changes at once.
-- |
-- | Note: It's not strictly necessary for any components in Conduit to subscribe to changes in the 
-- | logged-in user, because that only happens at login / logout; at these points, components are
-- | re-mounted anyway and will always be up-to-date with the latest state. Still, this is too 
-- | useful a pattern to leave out altogether!
loadUserEnv
  :: forall st act slots msg m r
   . MonadAff m
  => MonadAsk { currentUser :: Ref (Maybe Profile), userBus :: Bus.BusRW (Maybe Profile) | r } m
  => Navigate m
  => (Maybe Profile -> act)
  -> H.HalogenM st act slots msg m (Maybe Profile)
loadUserEnv handler = do 
  { currentUser, userBus } <- ask
  subscribeWithAction handler userBus 
  liftEffect $ Ref.read currentUser
