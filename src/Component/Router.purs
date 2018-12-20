module Conduit.Component.Router where

import Prelude

import Conduit.Api.Request (AuthUser)
import Conduit.AppM (Env)
import Conduit.Capability.Authenticate (class Authenticate, deleteAuth, readAuth)
import Conduit.Capability.LogMessages (class LogMessages, debugHush)
import Conduit.Capability.ManageResource (class ManageAuthResource, class ManageResource)
import Conduit.Capability.Navigate (class Navigate)
import Conduit.Capability.Now (class Now)
import Conduit.Data.Route (Route(..))
import Conduit.Page.Editor as Editor
import Conduit.Page.Home as Home
import Conduit.Page.Login as Login
import Conduit.Page.Profile (Tab(..))
import Conduit.Page.Profile as Profile
import Conduit.Page.Register as Register
import Conduit.Page.Settings as Settings
<<<<<<< HEAD
=======
import Conduit.Page.ViewArticle as ViewArticle
>>>>>>> impl
import Control.Monad.Reader (class MonadAsk)
import Data.Either (hush)
import Data.Either.Nested (Either7)
import Data.Functor.Coproduct.Nested (Coproduct7)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH

type State =
  { route :: Route 
  , authUser :: Maybe AuthUser
  }

data Query a
  = Initialize a
  | Navigate Route a

type ChildQuery = Coproduct7
  Home.Query
  Login.Query
  Register.Query
  Settings.Query
  Editor.Query
<<<<<<< HEAD
  (Const Void)
=======
  ViewArticle.Query
>>>>>>> impl
  Profile.Query

type ChildSlot = Either7
  Unit
  Unit
  Unit
  Unit
  Unit
  Unit
  Unit

component
  :: forall m
   . MonadAff m
  => MonadAsk Env m
  => Now m
  => LogMessages m
  => Authenticate m
  => Navigate m
  => ManageResource m
  => ManageAuthResource m
  => H.Component HH.HTML Query Unit Void m
component =
  H.lifecycleParentComponent
    { initialState: const { route: Home, authUser: Nothing } 
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just $ H.action Initialize
    , finalizer: Nothing
    }

  where 

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot m
  render { route, authUser } = case route of
    Home -> 
      HH.slot' CP.cp1 unit Home.component { authUser } absurd
    Login -> 
      HH.slot' CP.cp2 unit Login.component { authUser } absurd
    Register -> 
      HH.slot' CP.cp3 unit Register.component { authUser } absurd
    Settings -> 
      HH.slot' CP.cp4 unit Settings.component { authUser } absurd
    Editor -> 
      HH.slot' CP.cp5 unit Editor.component { authUser, slug: Nothing } absurd
    EditArticle slug -> 
      HH.slot' CP.cp5 unit Editor.component { authUser, slug: Just slug } absurd
<<<<<<< HEAD
    ViewArticle _ -> 
      HH.div_ []
=======
    ViewArticle slug -> 
      HH.slot' CP.cp6 unit ViewArticle.component { authUser, slug } absurd
>>>>>>> impl
    Profile username -> 
      HH.slot' CP.cp7 unit Profile.component { username, authUser, tab: ArticlesTab } absurd
    Favorites username -> 
      HH.slot' CP.cp7 unit Profile.component { username, authUser, tab: FavoritesTab } absurd
    Logout -> HH.text "Logging out..."
  
  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void m
  eval = case _ of
    Initialize a -> do
      authUser <- debugHush readAuth
      H.modify_ _ { authUser = authUser }
      pure a

    Navigate dest a -> do
      { route } <- H.get 
      case route of 
        Logout -> do
          deleteAuth 
          H.modify_ _ { route = Home, authUser = Nothing }
        _ -> do
          when (route /= dest) do
            authUser <- map hush readAuth
            H.modify_ _ { route = dest, authUser = authUser }
      pure a