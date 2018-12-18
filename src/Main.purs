module Main where

import Prelude

import Conduit.Api.Request (BaseURL(..))
import Conduit.AppM (Env, LogLevel(..), runAppM)
import Conduit.Component.Router as Router
import Data.Maybe (Maybe(..))
import Conduit.Data.Route (routeCodec)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Halogen (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import Routing.Duplex (parse)
import Routing.Hash (matchesWith)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody

  let environment :: Env
      environment = 
        { logLevel: Dev 
        , baseUrl: BaseURL "https://conduit.productionready.io/"
        }

      router :: H.Component HH.HTML Router.Query Unit Void Aff
      router = H.hoist (runAppM environment) Router.component
  
  driver <- runUI router unit body

  void $ liftEffect $ matchesWith (parse routeCodec) \old new ->
    when (old /= Just new) do
      launchAff_ $ driver.query $ Router.Navigate new unit
