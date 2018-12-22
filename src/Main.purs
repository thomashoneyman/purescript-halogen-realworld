module Main where

import Prelude

import Conduit.Api.Request (BaseURL(..), RequestType(..), readToken)
import Conduit.Api.Utils (mkAuthRequest, mkRequest)
import Conduit.AppM (Env, LogLevel(..), runAppM)
import Conduit.Component.Router as Router
import Conduit.Data.Endpoint (Endpoint(..))
import Conduit.Data.Route (routeCodec)
import Data.Argonaut.Decode (decodeJson, (.:))
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Data.Traversable (for_, traverse)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Ref as Ref
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

  -- Prepare the environment
  let baseUrl = BaseURL "https://conduit.productionready.io"

  currentUser <- liftEffect $ Ref.new Nothing
  mbToken <- liftEffect readToken

  -- If there is a token in local storage, request the associated user
  for_ mbToken \tok -> do
    res <- mkAuthRequest { endpoint: User, requestType: Get }
    -- (decodeJson <=< (_ .: "user") <=< decodeJson)
    pure unit
    -- liftEffect $ Ref.write (hush res) currentUser

  let environment :: Env
      environment = { logLevel: Dev, baseUrl, currentUser }

      router :: H.Component HH.HTML Router.Query Unit Void Aff
      router = H.hoist (runAppM environment) Router.component
  
  driver <- runUI router unit body

  void $ liftEffect $ matchesWith (parse routeCodec) \old new ->
    when (old /= Just new) do
      launchAff_ $ driver.query $ Router.Navigate new unit
  
  pure unit
