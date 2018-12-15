module Component.HTML.Utils where

import Prelude

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

css :: forall r i. String -> HH.IProp ( "class" :: String | r ) i
css = HP.class_ <<< HH.ClassName

-- There is no monoid instance for Halogen's HTML DSL, so this allows us to provide
-- an empty node when a condition is false 
guardHtml :: forall p i. Boolean -> HH.HTML p i -> HH.HTML p i
guardHtml = if _ then _ else HH.text ""