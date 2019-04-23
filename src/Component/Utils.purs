-- | Some utilities are useful across any component. We'll maintain them in this catch-all module.
module Conduit.Component.Utils where

import Prelude

import Conduit.Capability.Navigate (class Navigate, logout)
import Conduit.Data.Profile (Profile)
import Control.Monad.Reader (class MonadAsk, asks)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Halogen as H

-- | When a component has no queries or messages, it has no public interface and can be
-- | considered an "opaque" component. The only way for a parent to interact with the component
-- | is by sending input.
type OpaqueSlot = H.Slot (Const Void) Void

-- | Several components verify that a current user exists and, if there is none in state, log the 
-- | user out and redirect to the home page. This way, an inadvertent route to the settings page, 
-- | for example, is protected at initialization.
guardSession
  :: forall m r
   . MonadEffect m
  => MonadAsk { currentUser :: Ref (Maybe Profile) | r } m
  => Navigate m
  => m (Maybe Profile)
guardSession = do 
  asks _.currentUser >>= (Ref.read >>> liftEffect) >>= case _ of
    Nothing -> logout *> pure Nothing 
    Just profile -> pure (Just profile)

-- | Some components only reasonably need to mount if there is NO active session. This utility 
-- | redirects to the home page when there is an active session for a component like the 
-- | registration page.
guardNoSession
  :: forall m r
   . MonadEffect m
  => MonadAsk { currentUser :: Ref (Maybe Profile) | r } m
  => Navigate m
  => m Unit
guardNoSession = do 
  asks _.currentUser >>= (Ref.read >>> liftEffect) >>= case _ of
    Nothing -> pure unit
    Just _ -> logout *> pure unit