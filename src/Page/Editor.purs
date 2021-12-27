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
import Conduit.Data.Article (ArticleWithMetadata)
import Conduit.Data.Profile (Profile)
import Conduit.Data.Route (Route(..))
import Conduit.Form.Field as Field
import Conduit.Form.Validation (FormError)
import Conduit.Form.Validation as V
import Conduit.Store as Store
import Data.Const (Const)
import Data.Either (either)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Select (selectEq)
import Network.RemoteData (RemoteData(..), fromMaybe, toMaybe)
import Safe.Coerce (coerce)
import Slug (Slug)
import Type.Proxy (Proxy(..))

type Input = Maybe Slug

-- | See the Formless tutorial to learn how to build your own forms:
-- | https://github.com/thomashoneyman/purescript-halogen-formless

type Form :: (Type -> Type -> Type -> Type) -> Row Type
type Form f =
  ( title :: f String FormError String
  , description :: f String FormError String
  , body :: f String FormError String
  , tagList :: f (Array Tag) Void (Array String)
  )

type FormContext = F.FormContext (Form F.FieldState) (Form (F.FieldAction Action)) Input Action
type FormlessAction = F.FormlessAction (Form F.FieldState)

data Action
  = Initialize
  | Receive (Connected (Maybe Profile) FormContext)
  | Eval FormlessAction
  | HandleTagInput TagInput.Message

type State =
  { article :: RemoteData String ArticleWithMetadata
  , currentUser :: Maybe Profile
  , form :: FormContext
  }

type ChildSlots = (tagInput :: H.Slot (Const Void) TagInput.Message Unit)

component
  :: forall query output m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => ManageArticle m
  => H.Component query Input output m
component = F.formless { liftAction: Eval } mempty $ connect (selectEq _.currentUser) $ H.mkComponent
  -- Due to the use of `connect`, our input now also has `currentUser`
  -- in it, and due to the use of `formless`, our input also has all our form
  -- fields available, despite the component's only public input being the slug.
  { initialState: \{ context: currentUser, input: formContext } ->
      { article: NotAsked
      , currentUser
      , form: formContext
      }
  , render
  , eval: H.mkEval $ H.defaultEval
      { receive = Just <<< Receive
      , initialize = Just Initialize
      , handleAction = handleAction
      , handleQuery = handleQuery
      }
  }
  where
  handleAction :: Action -> H.HalogenM State Action ChildSlots (F.FormOutput _ output) m Unit
  handleAction = case _ of
    Initialize -> do
      mbSlug <- H.gets _.form.input
      for_ mbSlug \slug -> do
        H.modify_ _ { article = Loading }
        mbArticle <- getArticle slug
        H.modify_ _ { article = fromMaybe mbArticle }

        -- We'll pre-fill the form with values from the article being edited
        for_ mbArticle \{ title, description, body, tagList } -> do
          { formActions, fields } <- H.gets _.form
          let
            newFields = fields
              { title { value = title }
              , description { value = description }
              , body { value = body }
              , tagList { value = map Tag tagList }
              }
          handleAction $ formActions.setFields newFields

    Receive { input, context: currentUser } -> do
      state <- H.modify _ { currentUser = currentUser, form = input }
      when (input.input /= state.form.input) do
        handleAction Initialize

    Eval action ->
      F.eval action

    HandleTagInput msg -> case msg of
      TagInput.TagAdded _ set -> do
        tagList <- H.gets _.form.actions.tagList
        handleAction $ tagList.handleChange (Set.toUnfoldable set)
      TagInput.TagRemoved _ set -> do
        tagList <- H.gets _.form.actions.tagList
        handleAction $ tagList.handleChange (Set.toUnfoldable set)

  handleQuery :: forall a. F.FormQuery _ _ _ _ a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery = do
    let
      onSubmit article = do
        mbSlug <- H.gets _.form.input
        mbArticleWithMetadata <- case mbSlug of
          Nothing -> createArticle article
          Just slug -> updateArticle slug article
        let slug = _.slug <$> mbArticleWithMetadata
        H.modify_ _ { article = fromMaybe mbArticleWithMetadata, form { input = slug } }
        for_ slug (navigate <<< ViewArticle)

      validation :: { | Form F.FieldValidation }
      validation =
        { title: V.required >=> V.minLength 1
        , description: V.required >=> V.minLength 1
        , body: V.required >=> V.minLength 3
        , tagList: coerce >>> pure
        }

    F.handleSubmitValidate onSubmit F.validate validation

  render :: State -> H.ComponentHTML Action ChildSlots m
  render { currentUser, article, form: { formActions, fields, actions } } =
    HH.div_
      [ header currentUser Editor
      , container
          [ HH.form
              [ HE.onSubmit formActions.handleSubmit ]
              [ HH.fieldset_
                  [ Field.textInput { state: fields.title, action: actions.title }
                      [ HP.placeholder "Article Title" ]
                  , Field.textInput { state: fields.description, action: actions.description }
                      [ HP.placeholder "What's this article about?" ]
                  , HH.fieldset
                      [ css "form-group" ]
                      [ HH.textarea
                          [ css "form-control form-control-lg"
                          , HP.placeholder "Write your article (in markdown)"
                          , HP.value fields.body.value
                          , HP.rows 8
                          , HE.onValueInput actions.body.handleChange
                          , HE.onBlur actions.body.handleBlur
                          ]
                      , maybeElem (fields.body.result >>= either pure (const Nothing)) \err ->
                          HH.div
                            [ css "error-messages" ]
                            [ HH.text $ V.errorToString err ]
                      ]
                  , HH.slot (Proxy :: _ "tagInput") unit TagInput.component { tags: Set.fromFoldable fields.tagList.value } HandleTagInput
                  , Field.submitButton $ case toMaybe article of
                      Nothing -> "Publish"
                      Just _ -> "Commit changes"
                  ]
              ]
          ]
      ]
    where
    container form =
      HH.div
        [ css "editor-page" ]
        [ HH.div
            [ css "container page" ]
            [ HH.div
                [ css "row" ]
                [ HH.div
                    [ css "col-md-10 offset-md-1 col-xs-12" ]
                    form
                ]
            ]
        ]
