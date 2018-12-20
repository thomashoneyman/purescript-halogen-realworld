module Conduit.Page.ViewArticle where

import Prelude

import Conduit.Api.Request (AuthUser)
import Conduit.Capability.ManageResource (class ManageResource, getArticle)
import Conduit.Component.HTML.Footer (footer)
import Conduit.Component.HTML.Header (header)
import Conduit.Component.RawHTML as RawHTML
import Conduit.Data.Article (ArticleWithMetadata)
import Conduit.Data.Route (Route(..))
import Data.Either (either)
import Data.Lens (preview)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Network.RemoteData (RemoteData(..), _Success)
import Slug (Slug)

type State =
  { article :: RemoteData String ArticleWithMetadata
  , slug :: Slug
  , authUser :: Maybe AuthUser
  }

type Input =
  { slug :: Slug
  , authUser :: Maybe AuthUser
  }

data Query a
  = Initialize a

type ChildQuery = RawHTML.Query
type ChildSlot = Unit

component
  :: forall m
   . MonadAff m
  => ManageResource m
  => H.Component HH.HTML Query Input Void m
component =
  H.lifecycleParentComponent
    { initialState
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just $ H.action Initialize
    , finalizer: Nothing
    }

  where 

  initialState :: Input -> State
  initialState { slug, authUser } = { article: NotAsked, slug, authUser }

  eval :: Query ~> H.ParentDSL State Query ChildQuery Unit Void m
  eval = case _ of
    Initialize a -> do
      st <- H.modify _ { article = Loading }
      article <- getArticle st.slug
      H.modify_ _ { article = either Failure Success article }
      pure a      

  render :: State -> H.ParentHTML Query ChildQuery Unit m
  render state =
    HH.div_
      [ header state.authUser (ViewArticle state.slug)
      , HH.slot unit RawHTML.component { markdown } absurd
      , footer
      ]
    where
    markdown = fromMaybe "Failed to load article!" $ map _.body $ preview _Success state.article