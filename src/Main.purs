module Main where

import Prelude

import AppM (Env, LogLevel(..), runAppM)
import Component.Router as Router
import Data.Maybe (Maybe(..))
import Data.Route (routeCodec)
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
        , rootUrl: "https://conduit.productionready.io/"
        }

      router :: H.Component HH.HTML Router.Query Unit Void Aff
      router = H.hoist (runAppM environment) Router.component
  
  driver <- runUI router unit body

  void $ liftEffect $ matchesWith (parse routeCodec) \old new ->
    when (old /= Just new) do
      launchAff_ $ driver.query $ Router.Navigate new unit
