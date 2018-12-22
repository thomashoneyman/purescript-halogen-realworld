module Main where

import Prelude

import Affjax (printResponseFormatError, request)
import Conduit.Api.Request (BaseURL(..), RequestMethod(..), defaultRequest, readToken)
import Conduit.Api.Utils (decodeAt)
import Conduit.AppM (Env, LogLevel(..), runAppM)
import Conduit.Component.Router as Router
import Conduit.Data.Endpoint (Endpoint(..))
import Conduit.Data.Route (routeCodec)
import Data.Bifunctor (lmap)
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Ref as Ref
import Halogen (liftAff, liftEffect)
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

  -- If there is a token in local storage, request the associated user. This is verbose because
  -- it doesn't use our helper functions, but instead uses the `request` function from Affjax
  -- directly. We aren't in `AppM` yet!
  for_ mbToken \token -> do
    let opts = { endpoint: User, method: Get }
    res <- liftAff $ request $ defaultRequest baseUrl (Just token) opts
    let u = decodeAt "user" =<< lmap printResponseFormatError res.body
    liftEffect $ Ref.write (hush u) currentUser
    pure unit

  let 
    environment :: Env
    environment = { logLevel: Dev, baseUrl, currentUser }

    router :: H.Component HH.HTML Router.Query Unit Void Aff
    router = H.hoist (runAppM environment) Router.component
  
  driver <- runUI router unit body

  void $ liftEffect $ matchesWith (parse routeCodec) \old new ->
    when (old /= Just new) do
      launchAff_ $ driver.query $ Router.Navigate new unit
  
  pure unit
