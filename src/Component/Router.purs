-- | The `Router` component is the root of our Halogen application. Every other component is a 
-- | direct descendent of this component. We'll use the router to choose which component to render
-- | given a particular `Route` and to manage the user's location in the application.
-- |
-- | See `Main` to understand how this component is used as the root of the application.
module Conduit.Component.Router where

import Prelude

import Conduit.Capability.LogMessages (class LogMessages)
import Conduit.Capability.Navigate (class Navigate)
import Conduit.Capability.Now (class Now)
import Conduit.Capability.Resource.Article (class ManageArticle)
import Conduit.Capability.Resource.Comment (class ManageComment)
import Conduit.Capability.Resource.Tag (class ManageTag)
import Conduit.Capability.Resource.User (class ManageUser)
import Conduit.Component.Utils (OpaqueSlot)
import Conduit.Data.Profile (Profile)
import Conduit.Data.Route (Route(..))
import Conduit.Page.Editor as Editor
import Conduit.Page.Home as Home
import Conduit.Page.Login as Login
import Conduit.Page.Profile (Tab(..))
import Conduit.Page.Profile as Profile
import Conduit.Page.Register as Register
import Conduit.Page.Settings as Settings
import Conduit.Page.ViewArticle as ViewArticle
import Control.Monad.Reader (class MonadAsk)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Ref (Ref)
import Halogen as H
import Halogen.HTML as HH

type State =
  { route :: Route }

data Query a
  = Navigate Route a

type Input =
  Maybe Route

type ChildSlots = 
  ( home :: OpaqueSlot Unit
  , login :: OpaqueSlot Unit
  , register :: OpaqueSlot Unit
  , settings :: OpaqueSlot Unit
  , editor :: OpaqueSlot Unit
  , viewArticle :: OpaqueSlot Unit
  , profile :: OpaqueSlot Unit
  )

component
  :: forall m r
   . MonadAff m
  => MonadAsk { currentUser :: Ref (Maybe Profile) | r } m
  => Now m
  => LogMessages m
  => Navigate m
  => ManageUser m
  => ManageArticle m
  => ManageComment m
  => ManageTag m
  => H.Component HH.HTML Query Input Void m
component = H.mkComponent
  { initialState: \initialRoute -> { route: fromMaybe Home initialRoute } 
  , render
  , eval: H.mkEval $ H.defaultEval { handleQuery = handleQuery }
  }
  where 
  handleQuery :: forall a. Query a -> H.HalogenM State Void ChildSlots Void m (Maybe a)
  handleQuery = case _ of
    Navigate dest a -> do
      { route } <- H.get 
      when (route /= dest) do
        H.modify_ _ { route = dest }
      pure (Just a)

  render :: State -> H.ComponentHTML Void ChildSlots m
  render { route } = case route of
    Home -> 
      HH.slot (SProxy :: _ "home") unit Home.component unit absurd
    Login -> 
      HH.slot (SProxy :: _ "login") unit Login.component unit absurd
    Register -> 
      HH.slot (SProxy :: _ "register") unit Register.component unit absurd
    Settings -> 
      HH.slot (SProxy :: _ "settings") unit Settings.component unit absurd
    Editor -> 
      HH.slot (SProxy :: _ "editor") unit Editor.component { slug: Nothing } absurd
    EditArticle slug -> 
      HH.slot (SProxy :: _ "editor") unit Editor.component { slug: Just slug } absurd
    ViewArticle slug -> 
      HH.slot (SProxy :: _ "viewArticle") unit ViewArticle.component { slug } absurd
    Profile username -> 
      HH.slot (SProxy :: _ "profile") unit Profile.component { username, tab: ArticlesTab } absurd
    Favorites username -> 
      HH.slot (SProxy :: _ "profile") unit Profile.component { username, tab: FavoritesTab } absurd
