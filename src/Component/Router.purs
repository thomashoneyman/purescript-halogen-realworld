module Conduit.Component.Router where

import Prelude

import Conduit.Api.Request (AuthUser)
import Conduit.AppM (Env)
import Conduit.Capability.Authenticate (class Authenticate, deleteAuth, readAuth)
import Conduit.Capability.LogMessages (class LogMessages, debugHush)
import Conduit.Capability.ManageResource (class ManageAuthResource, class ManageResource)
import Conduit.Capability.Navigate (class Navigate)
import Conduit.Capability.Now (class Now)
import Conduit.Component.Page.Home as Home
import Conduit.Component.Page.Login as Login
import Conduit.Component.Page.Register as Register
import Conduit.Component.Page.Settings as Settings
import Conduit.Data.Route (Route(..))
import Control.Monad.Reader (class MonadAsk)
import Data.Const (Const)
import Data.Either (hush)
import Data.Either.Nested (Either9)
import Data.Functor.Coproduct.Nested (Coproduct9)
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

type ChildQuery = Coproduct9
  Home.Query
  Login.Query
  Register.Query
  Settings.Query
  (Const Void)
  (Const Void)
  (Const Void)
  (Const Void)
  (Const Void)

type ChildSlot = Either9
  Unit
  Unit
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
    Home -> HH.slot' CP.cp1 unit Home.component { authUser } absurd
    Login -> HH.slot' CP.cp2 unit Login.component { authUser } absurd
    Register -> HH.slot' CP.cp3 unit Register.component { authUser } absurd
    Settings -> HH.slot' CP.cp4 unit Settings.component { authUser } absurd
    Editor -> HH.div_ []
    EditArticle _ -> HH.div_ []
    ViewArticle _ -> HH.div_ []
    Profile _ -> HH.div_ []
    Favorites _ -> HH.div_ []
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