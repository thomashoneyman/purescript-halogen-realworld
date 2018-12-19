module Conduit.Component.HTML.Utils where

import Prelude

import Conduit.Data.Route (Route, routeCodec)
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Routing.Duplex (print)

css :: forall r i. String -> HH.IProp ( class :: String | r ) i
css = HP.class_ <<< HH.ClassName

-- We can ensure that we only create safe hashes by relying on our `Route` data type
safeHref :: forall r i. Route -> HH.IProp ( href :: String | r) i
safeHref = HP.href <<< append "#" <<< print routeCodec

-- PureScript is a strict language. If we want to conditionally display an element, then we
-- should hide the evaluation behind a function, which won't be evaluated right away, in order
-- to minimize the work performed each render.
whenElem :: forall p i. Boolean -> (Unit -> HH.HTML p i) -> HH.HTML p i
whenElem cond f = if cond then f unit else HH.text ""

-- Sometimes we need to deal with elements which may conditionally exist. Unfortunately, Halogen's
-- HTML types are not monoidal, so we can't easily generalize to any foldable type. Instead, we'll
-- specialize here to `Maybe`, and can make a separate implementation for other foldable structures
-- if needed later on.
maybeElem :: forall p i a. Maybe a -> (a -> HH.HTML p i) -> HH.HTML p i
maybeElem (Just x) f = f x
maybeElem _ _ = HH.text ""