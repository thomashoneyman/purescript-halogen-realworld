module Component.Router where

import Prelude

import AppM (Env)
import Capability.Authenticate (class Authenticate)
import Capability.LogMessages (class LogMessages, logDebug)
import Capability.ManageResource (class ManageAuthResource, class ManageResource, getTags)
import Capability.Navigate (class Navigate)
import Capability.Now (class Now)
import Control.Monad.Reader (class MonadAsk)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Route (Route(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type State =
  { route :: Route 
  }

data Query a
  = Navigate Route a
  | Click a

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

  render :: State -> H.ParentHTML Query (Const Void) Void m
  render { route } = case route of
    Home -> HH.div [ HE.onClick $ HE.input_ Click ] [ HH.text "click" ]
    Login -> HH.div_ []
    Register -> HH.div_ []
    Settings -> HH.div_ []
    Editor -> HH.div_ []
    EditArticle _ -> HH.div_ []
    ViewArticle _ -> HH.div_ []
    Profile _ -> HH.div_ []
    Favorites _ -> HH.div_ []
  
  eval :: Query ~> H.ParentDSL State Query (Const Void) Void Void m
  eval = case _ of
    Click a -> do
      res <- getTags
      logDebug $ show res
      pure a

    Navigate dest a -> do
      { route } <- H.get 
      when (route /= dest) do
        H.modify_ _ { route = dest }
      pure a