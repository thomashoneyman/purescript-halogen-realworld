-- | This module exports a pure HTML function to render a consistent footer throughout the app.
module Conduit.Component.HTML.Footer where

import Conduit.Component.HTML.Utils (css)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

footer :: forall i p. HH.HTML i p
footer =
  HH.footer_
    [ HH.div
        [ css "container" ]
        [ HH.a
            [ css "logo-font"
            , HP.href "/"
            ]
            [ HH.text "conduit" ]
        , HH.span
            [ css "attribution" ]
            [ HH.text "An interactive learning project from "
            , HH.a
                [ HP.href "https://thinkster.io" ]
                [ HH.text "Thinkster" ]
            , HH.text ". Code & design licensed under MIT. Implemented by "
            , HH.a
                [ HP.href "https://thomashoneyman.com" ]
                [ HH.text "Thomas Honeyman" ]
            , HH.text "."
            ]
        ]
    ]
