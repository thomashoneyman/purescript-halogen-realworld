-- | Halogen does not support writing an HTML string to the DOM. This component allows us to do this
-- | at a particular controlled HTML node.
module Conduit.Component.RawHTML where

import Prelude

import Conduit.Foreign.Marked (RawHTML, marked)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.HTML (HTMLElement)

-- | For an explanation of how to properly use the PureScript FFI with JavaScript, please see the 
-- | `src/Foreign/Marked.js` file and the `Conduit.Foreign.Marked` module.
foreign import unsafeSetInnerHTML :: HTMLElement -> RawHTML -> Effect Unit

type State =
  { elemRef :: H.RefLabel
  , markdown :: String
  }

type Input =
  { markdown :: String }

data Query a
  = SetInnerHTML a
  | Receive Input a

component :: forall m. MonadAff m => H.Component HH.HTML Query Input Void m
component = H.lifecycleComponent
  { initialState: \{ markdown } -> { elemRef: H.RefLabel "markdown", markdown } 
  , render
  , eval
  , receiver: HE.input Receive
  , initializer: Just $ H.action SetInnerHTML
  , finalizer: Nothing
  }
  where
  render :: State -> H.ComponentHTML Query
  render state = 
    HH.div 
      [ HP.ref state.elemRef ] 
      []

  eval :: Query ~> H.ComponentDSL State Query Void m
  eval = case _ of
    SetInnerHTML a -> do
      { elemRef } <- H.get
      mbElem <- H.getHTMLElementRef elemRef
      for_ mbElem \el -> do  
        { markdown } <- H.get
        H.liftEffect $ unsafeSetInnerHTML el $ marked markdown
      pure a
    
    Receive { markdown } a -> do
      H.modify_ _ { markdown = markdown }
      eval $ SetInnerHTML a
