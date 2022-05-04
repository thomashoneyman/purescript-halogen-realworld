-- | This module is the root of the entire application. It exports one function, `main`, which is
-- | the first thing run when a user loads up our application in the browser. This function is
-- | responsible for doing all the setup necessary for our app to run, which includes reading tokens
-- | from local storage and starting the Halogen application.
module Main where

import Prelude

import Affjax.Web (printError, request)
import Conduit.Api.Endpoint (Endpoint(..))
import Conduit.Api.Request (BaseURL(..), RequestMethod(..), defaultRequest, readToken)
import Conduit.AppM (runAppM)
import Conduit.Component.Router as Router
import Conduit.Data.Profile (Profile)
import Conduit.Data.Profile as Profile
import Conduit.Data.Route (routeCodec)
import Conduit.Store (LogLevel(..), Store)
import Data.Bifunctor (lmap)
import Data.Codec as Codec
import Data.Codec.Argonaut (printJsonDecodeError)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Halogen (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Routing.Duplex (parse)
import Routing.Hash (matchesWith)

-- | The `main` function is the first thing run in a PureScript application. In our case, that
-- | happens when a user loads our application in the browser.
-- |
-- | This function should be kept small -- its sole purpose is to run our Halogen application. To
-- | run the application, we'll first need to prepare some information our app needs to know about,
-- | like who the current user is (if there is one) or what our site's base URL is.
-- |
-- | There's one more thing we need to do. The `main` function in a PureScript application must
-- | run in the synchronous effects monad, `Effect`. But Halogen runs in the asynchronous effects
-- | monad, `Aff`, and our components run in our custom monad, `AppM`! We'll need to unify these
-- | different monads once we're ready to run the app.
main :: Effect Unit
main = HA.runHalogenAff do

  -- To run our Halogen app, we'll need two things:
  -- 1. An HTML element for Halogen to control
  -- 2. A component to mount at that HTML element, along with any arguments it requires

  -- First, we'll get a reference to the HTML element we want Halogen to attach to. Let's get a
  -- reference to the <body> tag as soon as it exists.
  body <- HA.awaitBody

  -- With our HTML reference ready to go, we can prepare to run our root Halogen component. The
  -- root component is the component that mounts all other components in the application. Since
  -- the router component decides what content should display depending on the user's location in
  -- the app, it's common practice to use this component as the root.

  -- Our router component requires some information about its environment in order to run, so let's
  -- get that handled before we do anything else.

  -- Our environment is a small record type, `Store`, defined in the `Conduit.Store` module. It
  -- requires three fields: the profile of the currently-authenticated user (if there is one), the
  -- base URL of the application, and the log level.

  -- This is a small MVP, so we'll just define pure values like our base URL and log level as
  -- constants. But it's also common to read configuration like this from the build environment.
  let
    baseUrl = BaseURL "https://api.realworld.io"
    logLevel = Dev

  -- We have two of the three fields we need to create our central state. The
  -- third field is the current user profile. We'll try to retrieve it in a few steps.
  -- First, we'll use `readToken` to read an authentication token out of local
  -- storage. If we can read one, we'll use it to try and get a new user (which
  -- will fail if the token isn't valid).
  currentUser :: Maybe Profile <- (liftEffect readToken) >>= case _ of
    Nothing ->
      pure Nothing

    Just token -> do
      let requestOptions = { endpoint: User, method: Get }
      res <- request $ defaultRequest baseUrl (Just token) requestOptions

      let
        user :: Either String Profile
        user = case res of
          Left e ->
            Left (printError e)
          Right v -> lmap printJsonDecodeError do
            u <- Codec.decode (CAR.object "User" { user: CA.json }) v.body
            CA.decode Profile.profileCodec u.user

      pure $ hush user

  -- We now have the three pieces of information necessary to configure our app. Let's create
  -- a record that matches the `Store` type our application requires by filling in these three
  -- fields. If our environment type ever changes, we'll get a compiler error here.
  let
    initialStore :: Store
    initialStore = { baseUrl, logLevel, currentUser }

  -- With our app environment ready to go, we can prepare the router to run as our root component.
  --
  -- But wait! Our router is configured to run in a monad that supports all our capabilities like
  -- navigation, API requests, and logging. More concretely, we'll run the application in our
  -- custom `AppM` monad, which supports all these.
  --
  -- But Halogen only knows how to run components in the `Aff` (asynchronous effects) monad. `Aff`
  -- has no idea how to interpret our capabilities. We need a way to change our router component so
  -- that it runs in `Aff` instead of `AppM`. We can do that with `runAppM`:
  rootComponent <- runAppM initialStore Router.component

  -- Now we have the two things we need to run a Halogen application: a reference to an HTML element
  -- and the component to run there.
  --
  -- To run a Halogen application, use the `runUI` function. It accepts the component to run, arguments
  -- to provide to the component (in our case, the landing page), and the reference to an HTML element.
  -- It will start the Halogen application and return a record with two fields:
  --
  -- `query`, which lets us send queries down to the root component
  -- `subscribe`, which lets us listen and react to messages output by the root component
  --
  -- Note: Since our root component is our router, the "queries" and "messages" above refer to the
  -- `Query` and `Message` types defined in the `Conduit.Router` module. Only those queries and
  -- messages can be used, or else you'll get a compiler error.
  halogenIO <- runUI rootComponent unit body

  -- Fantastic! Our app is running and we're almost done. All that's left is to notify the router
  -- any time the location changes in the URL.
  --
  -- We're using hash-based routing, so we'll use the `matchesWith` function from `Routing.Hash` to
  -- listen for changes in the hash and parse the result (using our routing codec, `routeCodec`,
  -- along with the `parse` function from `Routing.Duplex`). Any time we parse a new location we'll
  -- trigger a `Navigate` query in the router.
  --
  -- If you feel confused by what's going on here, I'd recommend the `purescript-routing` and
  -- `purescript-routing-duplex` guides:
  --
  -- https://github.com/slamdata/purescript-routing/blob/v8.0.0/GUIDE.md
  -- https://github.com/natefaubion/purescript-routing-duplex/blob/v0.2.0/README.md
  void $ liftEffect $ matchesWith (parse routeCodec) \old new ->
    when (old /= Just new) $ launchAff_ do
      _response <- halogenIO.query $ H.mkTell $ Router.Navigate new
      pure unit
