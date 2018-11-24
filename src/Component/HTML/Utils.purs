module Component.HTML.Utils where

import Prelude

import Halogen as HP
import Halogen.HTML.Properties as HH

css :: forall r p. String -> HH.IProp ( "class" :: String | r ) p
css = HH.class_ <<< HP.ClassName