-- | The editor allows users to write new articles or edit existing ones. Users write markdown and
-- | can include tags for their articles; when articles are displayed, the markdown is rendered as
-- | HTML content.
module Conduit.Page.Editor where

import Prelude

import Conduit.Capability.Navigate (class Navigate, navigate)
import Conduit.Capability.Resource.Article (class ManageArticle, createArticle, getArticle, updateArticle)
import Conduit.Component.HTML.Header (header)
import Conduit.Component.HTML.Utils (css, maybeElem)
import Conduit.Component.TagInput (Tag(..))
import Conduit.Component.TagInput as TagInput
import Conduit.Component.Utils (busEventSource)
import Conduit.Data.Article (ArticleWithMetadata, Article)
import Conduit.Data.Profile (Profile)
import Conduit.Data.Route (Route(..))
import Conduit.Env (UserEnv)
import Conduit.Form.Field as Field
import Conduit.Form.Validation (errorToString)
import Conduit.Form.Validation as V
import Control.Monad.Reader (class MonadAsk, asks)
import Data.Const (Const)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (class Newtype, unwrap)
import Data.Set as Set
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Ref as Ref
import Formless as F
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..), fromMaybe, toMaybe)
import Slug (Slug)

data Action
  = Initialize
  | HandleUserBus (Maybe Profile)
  | HandleEditor Article

type State =
  { article :: RemoteData String ArticleWithMetadata
  , slug :: Maybe Slug
  , currentUser :: Maybe Profile
  }

type Input =
  { slug :: Maybe Slug }

type ChildSlots =
  ( formless :: F.Slot EditorFields (Const Void) FormChildSlots Article Unit )

component
  :: forall m r
   . MonadAff m
  => MonadAsk { userEnv :: UserEnv | r } m
  => Navigate m
  => ManageArticle m
  => H.Component HH.HTML (Const Void) Input Void m
component = H.mkComponent
  { initialState: \{ slug } -> { article: NotAsked, currentUser: Nothing, slug }
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }
  where
  handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
  handleAction = case _ of
    Initialize ->  do
      { currentUser, userBus } <- asks _.userEnv
      _ <- H.subscribe (HandleUserBus <$> busEventSource userBus)
      mbProfile <- liftEffect $ Ref.read currentUser
      st <- H.modify _ { currentUser = mbProfile }
      for_ st.slug \slug -> do
        H.modify_ _ { article = Loading }
        mbArticle <- getArticle slug
        H.modify_ _ { article = fromMaybe mbArticle }

        -- We'll pre-fill the form with values from the article being edited
        for_ mbArticle \{ title, description, body, tagList } -> do
          let newFields = F.wrapInputFields { title, description, body, tagList: map Tag tagList }
          _ <- H.query F._formless unit $ F.asQuery $ F.loadForm newFields
          pure unit

    HandleUserBus mbProfile ->
      H.modify_ _ { currentUser = mbProfile }

    HandleEditor article -> do
      st <- H.get
      mbArticleWithMetadata <- case st.slug of
        Nothing -> createArticle article
        Just s -> updateArticle s article
      let
        slug = _.slug <$> mbArticleWithMetadata
      H.modify_ _ { article = fromMaybe mbArticleWithMetadata, slug = slug }
      for_ slug (navigate <<< ViewArticle)

  render :: State -> H.ComponentHTML Action ChildSlots m
  render { currentUser, article } =
    HH.div_
      [ header currentUser Editor
      , HH.div
        [ css "editor-page" ]
        [ HH.div
            [ css "container page" ]
            [ HH.div
              [ css "row" ]
              [ HH.div
                [ css "col-md-10 offset-md-1 col-xs-12" ]
                [ HH.slot F._formless unit (formComponent (toMaybe article)) unit (Just <<< HandleEditor) ]
              ]
            ]
        ]
      ]

-----
-- Form

-- | See the Formless tutorial to learn how to build your own forms:
-- | https://github.com/thomashoneyman/purescript-halogen-formless

newtype EditorFields r f = EditorFields (r
  ( title :: f V.FormError String String
  , description :: f V.FormError String String
  , body :: f V.FormError String String
  , tagList :: f Void (Array Tag) (Array String)
  ))
derive instance newtypeEditorFields :: Newtype (EditorFields r f) _

type FormChildSlots =
  ( tagInput :: H.Slot (Const Void) TagInput.Message Unit )

data FormAction
  = HandleTagInput TagInput.Message

formComponent
  :: forall m. MonadAff m
  => Maybe ArticleWithMetadata
  -> F.Component EditorFields (Const Void) FormChildSlots Unit Article m
formComponent mbArticle = F.component formInput $ F.defaultSpec
  { render = render
  , handleAction = handleAction
  , handleEvent = handleEvent
  }
  where
  formInput :: Unit -> F.Input' EditorFields m
  formInput _ =
    { validators: EditorFields
        { title: V.required >>> V.minLength 1
        , description: V.required >>> V.minLength 1
        , body: V.required >>> V.minLength 3
        , tagList: F.hoistFn_ (map unwrap)
        }
    , initialInputs: Nothing
    }

  proxies = F.mkSProxies (F.FormProxy :: _ EditorFields)

  handleEvent = F.raiseResult

  handleAction = case _ of
    HandleTagInput msg -> case msg of
      TagInput.TagAdded _ set -> do
        eval $ F.set proxies.tagList (Set.toUnfoldable set)
        pure unit
      TagInput.TagRemoved _ set -> do
        eval $ F.set proxies.tagList (Set.toUnfoldable set)
        pure unit
    where
    eval act = F.handleAction handleAction handleEvent act

  render st@{ form } =
    HH.form_
      [ HH.fieldset_
        [ title
        , description
        , body
        , HH.slot (SProxy :: _ "tagInput") unit TagInput.component unit handler
        , Field.submit $ if isJust mbArticle then "Commit changes" else "Publish"
        ]
      ]
    where
    handler = Just <<< F.injAction <<< HandleTagInput

    title =
      Field.input proxies.title form
        [ HP.placeholder "Article Title", HP.type_ HP.InputText ]

    description =
      Field.input proxies.description form
        [ HP.placeholder "What's this article about?", HP.type_ HP.InputText ]

    body =
      HH.fieldset
        [ css "form-group" ]
        [ HH.textarea
            [ css "form-control form-control-lg"
            , HP.placeholder "Write your article (in markdown)"
            , HP.value $ F.getInput proxies.body form
            , HP.rows 8
            , HE.onValueInput $ Just <<< F.setValidate proxies.body
            ]
          , maybeElem (F.getError proxies.body form) \err ->
              HH.div
                [ css "error-messages" ]
                [ HH.text $ errorToString err ]
          ]
