-- | Conduit has little functionality to warrant encapsulated state, but the tag input is one.
-- | This component manages a text input that supports key events to manage a list of tags on
-- | an article.
module Conduit.Component.TagInput where

import Prelude

import Conduit.Component.HTML.Utils (css)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Set (Set)
import Data.Set as Set
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event (preventDefault)
import Web.UIEvent.KeyboardEvent (KeyboardEvent, code, toEvent)

data Query a
  = HandleInput String a
  | HandleKey KeyboardEvent a
  | RemoveTag Tag a

type State =
  { text :: String
  , tags :: Set Tag
  }

newtype Tag = Tag String

derive instance newtypeTag :: Newtype Tag _
derive instance eqTag :: Eq Tag
derive instance ordTag :: Ord Tag

data Message 
  = TagAdded Tag (Set Tag) 
  | TagRemoved Tag (Set Tag) 

component :: forall m. MonadEffect m => H.Component HH.HTML Query Unit Message m
component = 
  H.component
    { initialState: \_ -> { tags: Set.empty, text: "" }
    , render
    , eval
    , receiver: const Nothing
    }
  where
  render :: State -> H.ComponentHTML Query
  render { text, tags } =
    HH.fieldset
      [ css "form-group" ]
      [ HH.input 
        [ css "form-control"
        , HP.type_ HP.InputText 
        , HP.placeholder "Enter tags"
        , HP.value text
        , HE.onValueInput $ HE.input HandleInput
        , HE.onKeyDown $ HE.input HandleKey
        ]
      , HH.div
        [ css "tag-list" ]
        (map renderTag (Set.toUnfoldable tags))
      ]
  
  renderTag :: Tag -> H.ComponentHTML Query
  renderTag tag =
    HH.span
      [ css "tag-default tag-pill" ]
      [ HH.i
        [ css "ion-close-round" 
        , HE.onClick $ HE.input_ $ RemoveTag tag
        ]
        [ ]
      , HH.text (unwrap tag)
      ]

  eval :: Query ~> H.ComponentDSL State Query Message m
  eval = case _ of 
    HandleInput str a -> a <$ do
      H.modify_ _ { text = str }

    HandleKey ev a -> a <$ do
      case code ev of
        "Enter" -> do 
          H.liftEffect $ preventDefault (toEvent ev)
          st <- H.get
          when (st.text /= "") do
            newState <- H.modify _ { tags = Set.insert (Tag st.text) st.tags, text = "" }
            H.raise $ TagAdded (Tag st.text) newState.tags
        _ -> pure unit

    RemoveTag tag a -> a <$ do
      st <- H.modify \s -> s { tags = Set.delete tag s.tags }
      H.raise $ TagRemoved tag st.tags