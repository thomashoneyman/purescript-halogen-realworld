-- | Some utilities are useful across any component. We'll maintain them in this catch-all module.
module Conduit.Component.Utils where

import Prelude

import Control.Monad.Rec.Class (forever)
import Effect.Aff (forkAff)
import Effect.Aff.Bus as Bus
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Subscription as HS
import Halogen.Subscription (Emitter)

-- | When a component has no queries or messages, it has no public interface and can be
-- | considered an "opaque" component. The only way for a parent to interact with the
-- | component is by sending input.
type OpaqueSlot slot = forall query. H.Slot query Void slot

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
-- |     { currentUser, userBus } <- ask
-- |     _ <- H.subscribe =<< map HandleBus <$> busEventEmitter userBus
-- |     mbProfile <- liftEffect $ Ref.read currentUser
-- |     ...
-- |
-- |   HandleBus busMessage -> do
-- |     ...
-- | ```
busEventEmitter :: forall m r act. MonadAff m => Bus.BusR' r act -> m (Emitter act)
busEventEmitter bus = do
  { emitter, listener } <- H.liftEffect HS.create
  _ <- H.liftAff $ forkAff $ forever do
    action <- Bus.read bus
    H.liftEffect $ HS.notify listener action
  pure emitter

