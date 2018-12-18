module Conduit.Component.HTML.Utils where

import Prelude

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

css :: forall r i. String -> HH.IProp ( "class" :: String | r ) i
css = HP.class_ <<< HH.ClassName

-- PureScript is a strict language. If we want to conditionally display an element, then we
-- should hide the evaluation behind a function, which won't be evaluated right away, in order
-- to minimize the work performed each render.
whenElem :: forall p i. Boolean -> (Unit -> HH.HTML p i) -> HH.HTML p i
whenElem cond f = if cond then f unit else HH.text ""