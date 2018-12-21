module Conduit.Page.Editor where

import Prelude

import Conduit.Api.Request (AuthUser)
import Conduit.Capability.ManageResource (class ManageAuthResource, class ManageResource, createArticle, getArticle, updateArticle)
import Conduit.Capability.Navigate (class Navigate, navigate)
import Conduit.Component.HTML.Header (header)
import Conduit.Component.HTML.Utils (css)
import Conduit.Component.TagInput (Tag(..))
import Conduit.Component.TagInput as TagInput
import Conduit.Data.Article (ArticleWithMetadata)
import Conduit.Data.Route (Route(..))
import Conduit.Form.Field as Field
import Conduit.Form.Validation as V
import Data.Either (either)
import Data.Foldable (for_)
import Data.Lens (_Right, preview)
import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.Newtype (class Newtype, unwrap)
import Data.Set as Set
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Formless as Formless
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..), toMaybe)
import Slug (Slug)

data Query a
  = Initialize a
  | HandleForm (F.Message Query EditorFields) a
  | HandleTagInput TagInput.Message a

type State =
  { article :: RemoteData String ArticleWithMetadata
  , slug :: Maybe Slug
  , authUser :: Maybe AuthUser 
  }

type Input =
  { slug :: Maybe Slug
  , authUser :: Maybe AuthUser 
  }

type ChildQuery m = F.Query Query TagInput.Query Unit EditorFields m
type ChildSlot = Unit

component 
  :: forall m
   . MonadAff m 
  => Navigate m
  => ManageResource m
  => ManageAuthResource m
  => H.Component HH.HTML Query Input Void m
component = 
  H.lifecycleParentComponent
    { initialState: \{ authUser, slug } -> { article: NotAsked, authUser, slug }
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just $ H.action Initialize
    , finalizer: Nothing
    }
  where
  eval :: Query ~> H.ParentDSL State Query (ChildQuery m) Unit Void m
  eval = case _ of
    Initialize a ->  do
      st <- H.get
      when (isNothing st.authUser) (navigate Logout)
      for_ st.slug \slug -> do
        H.modify_ _ { article = Loading }
        eitherArticle <- getArticle slug
        H.modify_ _ { article = either Failure Success eitherArticle }

        -- We'll pre-fill the form with values from the article being edited
        for_ eitherArticle \{ title, description, body, tagList } -> do
          let 
            newFields = F.wrapInputFields 
              { title
              , description
              , body
              , tagList: map Tag tagList
              }
          H.query unit $ F.loadForm_ newFields
      pure a

    HandleForm msg a -> case msg of
      F.Submitted formOutputs -> do
        let res = F.unwrapOutputFields formOutputs
        st <- H.get
        articleWithMetadata <- case st.slug of
          Nothing -> createArticle res
          Just s -> updateArticle s res
        let 
          slug = _.slug <$> preview _Right articleWithMetadata
        H.modify_ _ { article = either Failure Success articleWithMetadata, slug = slug }
        for_ slug (navigate <<< ViewArticle)       
        pure a
      _ -> pure a
    
    HandleTagInput msg a -> case msg of
      TagInput.TagAdded _ set -> a <$ do
        H.query unit $ F.set_ proxies.tagList (Set.toUnfoldable set)
      TagInput.TagRemoved _ set -> a <$ do
        H.query unit $ F.set_ proxies.tagList (Set.toUnfoldable set)
    
  render :: State -> H.ParentHTML Query (ChildQuery m) ChildSlot m
  render { authUser, article } =
    container
      [ HH.slot unit Formless.component 
          { initialInputs: F.mkInputFields formProxy
          , validators
          , render: renderFormless (toMaybe article)
          } 
          (HE.input HandleForm)
      ]
    where
    container html =
      HH.div_
        [ header authUser Editor
        , HH.div
          [ css "editor-page" ]
          [ HH.div
              [ css "container page" ]
              [ HH.div
                [ css "row" ]
                [ HH.div
                  [ css "col-md-10 offset-md-1 col-xs-12" ]
                  html
                ]
              ]
          ]
        ]

-----
-- Form

newtype EditorFields r f = EditorFields (r
  ( title :: f V.FormError String String
  , description :: f V.FormError String String
  , body :: f V.FormError String String
  , tagList :: f Void (Array Tag) (Array String)
  ))
derive instance newtypeEditorFields :: Newtype (EditorFields r f) _

formProxy :: F.FormProxy EditorFields
formProxy = F.FormProxy

proxies :: F.SProxies EditorFields
proxies = F.mkSProxies formProxy

validators :: forall m. Monad m => EditorFields Record (F.Validation EditorFields m)
validators = EditorFields
  { title: V.required >>> V.minLength 1
  , description: V.required >>> V.minLength 1
  , body: V.required >>> V.minLength 3
  , tagList: F.hoistFn_ (map unwrap)
  }

renderFormless 
  :: forall m
   . MonadAff m 
  => Maybe ArticleWithMetadata 
  -> F.State EditorFields m 
  -> F.HTML Query TagInput.Query Unit EditorFields m
renderFormless mbArticle fstate =
  HH.form_
    [ HH.fieldset_
      [ title
      , description
      , body
      , HH.slot unit TagInput.component unit (HE.input $ F.raise <<< H.action <<< HandleTagInput)
      , Field.submit $ if isJust mbArticle then "Commit changes" else "Publish"
      ]
    ]
  where
  title =
    Field.input proxies.title fstate.form
      [ HP.placeholder "Article Title", HP.type_ HP.InputText ]

  description = 
    Field.input proxies.description fstate.form
      [ HP.placeholder "What's this article about?", HP.type_ HP.InputText ]

  body = 
    HH.fieldset
      [ css "form-group" ]
      [ HH.textarea 
        [ css "form-control form-control-lg"
        , HP.placeholder "Write your article (in markdown)"
        , HP.value $ F.getInput proxies.body fstate.form
        , HP.rows 8
        , HE.onValueInput $ HE.input $ F.setValidate proxies.body
        ]
      ] 
