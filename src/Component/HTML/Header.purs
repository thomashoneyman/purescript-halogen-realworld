-- | This module exports a pure HTML function to render a consistent header throughout the app.
module Conduit.Component.HTML.Header where

import Prelude

import Conduit.Component.HTML.Utils (css, maybeElem, safeHref, whenElem)
import Conduit.Data.Avatar as Avatar
import Conduit.Data.Profile (ProfileRep)
import Conduit.Data.Route (Route(..))
import Conduit.Data.Username as Username
import Data.Maybe (Maybe, isJust, isNothing)
import Data.Monoid (guard)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

-- | Our header will be a pure render function, but we'll require a route as an argument so we can
-- | judge whether a link should display active or not. We'll allow for any profile record type so
-- | long as it has our core fields -- this makes the header reusable across pages despite which
-- | variation on `Profile` they use.
header :: forall i p r. Maybe { | ProfileRep r } -> Route -> HH.HTML i p
header currentUser route =
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
                [ HH.text "Home" ]
            , whenElem (isJust currentUser) \_ ->
                navItem Editor
                  [ HH.i
                      [ css "ion-compose" ]
                      [ HH.text " New Post" ]
                  ]
            , whenElem (isJust currentUser) \_ ->
                navItem Settings
                  [ HH.i
                      [ css "ion-gear-a" ]
                      [ HH.text " Settings" ]
                  ]
            , maybeElem currentUser \profile ->
                navItem (Profile profile.username)
                  [ HH.img
                      [ css "user-pic"
                      , HP.src $ Avatar.toStringWithDefault profile.image
                      ]
                  , HH.text $ Username.toString profile.username
                  ]
            , whenElem (isNothing currentUser) \_ ->
                navItem Login
                  [ HH.text "Log in" ]
            , whenElem (isNothing currentUser) \_ ->
                navItem Register
                  [ HH.text "Sign up" ]
            ]
        ]
    ]

  where

  navItem r html =
    HH.li
      [ css "nav-item" ]
      [ HH.a
          [ css $ "nav-link" <> guard (route == r) " active"
          , safeHref r
          ]
          html
      ]
