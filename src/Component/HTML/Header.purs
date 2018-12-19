module Conduit.Component.HTML.Header where

import Prelude

import Conduit.Api.Request (AuthUser)
import Conduit.Component.HTML.Utils (css, safeHref, whenElem)
import Conduit.Data.Route (Route(..))
import Data.Maybe (Maybe, isNothing, isJust)
import Data.Monoid (guard)
import Halogen.HTML as HH

-- Our header will be a pure render function, but we'll require a route as an 
-- argument so we can judge whether a link should display active or not. We'll 
-- also take a query so that we can trigger navigation actions from the HTML. 

header :: forall i p. Maybe AuthUser -> Route -> HH.HTML i p
header authUser route =
  HH.nav
    [ css "navbar navbar-light" ]
    [ HH.div
      [ css "container" ]
      [ HH.a
        [ css "navbar-brand" 
        , safeHref Home
        ]
        [ HH.text "conduit" ]
      , HH.ul
        [ css "nav navbar-nav pull-xs-right" ]
        [ navItem Home 
        , whenElem (isJust authUser) \_ -> 
            navItem Editor
        , whenElem (isJust authUser) \_ -> 
            navItem Settings
        , whenElem (isNothing authUser) \_ ->
            navItem Login
        , whenElem (isNothing authUser) \_ ->
            navItem Register
        ]
      ]
    ]

  where

  navItem r = 
    HH.li
      [ css "nav-item" ]
      [ HH.a
        [ css $ "nav-link" <> guard (route == r) " active"
        , safeHref r
        ]
        [ displayRoute r ]
      ]

  displayRoute = case _ of
    Home -> 
      HH.text "Home"
    Editor -> 
      HH.i 
        [ css "ion-compose" ]
        [ HH.text " New Post" ]
    Settings -> 
      HH.i
        [ css "ion-gear-a" ]
        [ HH.text " Settings" ]
    Register -> 
      HH.text "Sign up"
    x -> 
      HH.text $ show x
