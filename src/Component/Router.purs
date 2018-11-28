module Component.Router where

import Prelude

import AppM (Env)
import Capability.Authenticate (class Authenticate)
import Capability.LogMessages (class LogMessages)
import Capability.ManageResource (class ManageAuthResource, class ManageResource)
import Capability.Navigate (class Navigate)
import Capability.Now (class Now)
import Component.Page.Home as Home
import Control.Monad.Reader (class MonadAsk)
import Data.Maybe (Maybe(..))
import Data.Route (Route(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH

type State =
  { route :: Route 
  }

data Query a
  = Navigate Route a

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
  H.parentComponent
    { initialState: const { route: Home } 
    , render
    , eval
    , receiver: const Nothing
    }

  where 

  render :: State -> H.ParentHTML Query Home.Query Unit m
  render { route } = case route of
    Home -> HH.slot unit Home.component unit (const Nothing)
    Login -> HH.div_ []
    Register -> HH.div_ []
    Settings -> HH.div_ []
    Editor -> HH.div_ []
    EditArticle _ -> HH.div_ []
    ViewArticle _ -> HH.div_ []
    Profile _ -> HH.div_ []
    Favorites _ -> HH.div_ []
  
  eval :: Query ~> H.ParentDSL State Query Home.Query Unit Void m
  eval = case _ of
    Navigate dest a -> do
      { route } <- H.get 
      when (route /= dest) do
        H.modify_ _ { route = dest }
      pure a
