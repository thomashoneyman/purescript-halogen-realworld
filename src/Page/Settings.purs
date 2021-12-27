-- | The settings page lets users change data about their account, like their email or password,
-- | as well as their publicly-viewable profile information.
module Conduit.Page.Settings where

import Prelude

import Conduit.Capability.Navigate (class Navigate, logout)
import Conduit.Capability.Resource.User (class ManageUser, getCurrentUser, updateUser)
import Conduit.Component.HTML.Header (header)
import Conduit.Component.HTML.Utils (css)
import Conduit.Data.Avatar (Avatar)
import Conduit.Data.Avatar as Avatar
import Conduit.Data.Email (Email)
import Conduit.Data.Profile (ProfileWithEmail)
import Conduit.Data.Route (Route(..))
import Conduit.Data.Username (Username)
import Conduit.Data.Username as Username
import Conduit.Form.Field as Field
import Conduit.Form.Validation (FormError)
import Conduit.Form.Validation as V
import Data.Lens (preview)
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Newtype (unwrap)
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..), _Success, fromMaybe)

-- | See the Formless tutorial to learn how to build your own forms:
-- | https://github.com/thomashoneyman/purescript-halogen-formless

type Form :: (Type -> Type -> Type -> Type) -> Row Type
type Form f =
  ( image :: f String FormError (Maybe Avatar)
  , username :: f String FormError Username
  , bio :: f String FormError (Maybe String)
  , email :: f String FormError Email
  , password :: f String FormError (Maybe String)
  )

type FormContext = F.FormContext (Form F.FieldState) (Form (F.FieldAction Action)) Unit Action
type FormlessAction = F.FormlessAction (Form F.FieldState)

data Action
  = Initialize
  | Receive FormContext
  | Eval FormlessAction
  | LogUserOut

type State =
  { profile :: RemoteData String ProfileWithEmail
  , form :: FormContext
  }

component
  :: forall query output m
   . MonadAff m
  => Navigate m
  => ManageUser m
  => H.Component query Unit output m
component = F.formless { liftAction: Eval } mempty $ H.mkComponent
  { initialState: \form -> { profile: NotAsked, form }
  , render
  , eval: H.mkEval $ H.defaultEval
      { initialize = Just Initialize
      , receive = Just <<< Receive
      , handleAction = handleAction
      , handleQuery = handleQuery
      }
  }
  where
  handleAction = case _ of
    Initialize -> do
      H.modify_ _ { profile = Loading }
      mbProfileWithEmail <- getCurrentUser
      H.modify_ _ { profile = fromMaybe mbProfileWithEmail }

      -- if the profile couldn't be located then something horrible has gone wrong
      -- and we should log the user out
      case mbProfileWithEmail of
        Nothing -> logout
        Just profile -> do
          { formActions, fields } <- H.gets _.form

          let
            newFields = fields
              { image { value = Maybe.fromMaybe "" $ Avatar.toString <$> profile.image }
              , username { value = Username.toString profile.username }
              , bio { value = Maybe.fromMaybe "" profile.bio }
              , email { value = unwrap profile.email }
              , password { value = "" }
              }

          handleAction $ formActions.setFields newFields

    Receive context ->
      H.modify_ _ { form = context }

    Eval action ->
      F.eval action

    LogUserOut ->
      logout

  handleQuery :: forall a. F.FormQuery _ _ _ _ a -> H.HalogenM _ _ _ _ m (Maybe a)
  handleQuery = do
    let
      onSubmit :: { | Form F.FieldOutput } -> H.HalogenM _ _ _ _ m Unit
      onSubmit fields = do
        updateUser fields
        mbProfileWithEmail <- getCurrentUser
        H.modify_ _ { profile = fromMaybe mbProfileWithEmail }

      validation :: { | Form F.FieldValidation }
      validation =
        { image: V.toOptional V.avatarFormat
        , username: V.required >=> V.minLength 3 >=> V.maxLength 20 >=> V.usernameFormat
        , bio: pure <<< Just
        , email: V.required >=> V.minLength 3 >=> V.maxLength 50 >=> V.emailFormat
        , password: V.toOptional $ V.minLength 3 >=> V.maxLength 20
        }

    F.handleSubmitValidate onSubmit F.validate validation

  render :: State -> H.ComponentHTML Action () m
  render { profile, form: { formActions, fields, actions } } =
    container
      [ HH.h1
          [ css "text-xs-center" ]
          [ HH.text "Your Settings" ]
      , HH.form
          [ HE.onSubmit formActions.handleSubmit ]
          [ HH.fieldset_
              [ Field.textInput
                  { state: fields.image, action: actions.image }
                  [ HP.placeholder "URL of profile picture" ]
              , Field.textInput
                  { state: fields.username, action: actions.username }
                  [ HP.placeholder "Your name" ]
              , Field.textarea
                  { state: fields.bio, action: actions.bio }
                  [ HP.placeholder "Short bio about you" ]
              , Field.textInput
                  { state: fields.email, action: actions.email }
                  [ HP.placeholder "Email"
                  , HP.type_ HP.InputEmail
                  ]
              , Field.textInput
                  { state: fields.password, action: actions.password }
                  [ HP.placeholder "Password"
                  , HP.type_ HP.InputPassword
                  ]
              , Field.submitButton "Update settings"
              ]
          ]
      , HH.hr_
      , HH.button
          [ css "btn btn-outline-danger"
          , HE.onClick \_ -> LogUserOut
          ]
          [ HH.text "Log out" ]
      ]
    where
    container html =
      HH.div_
        [ header (preview _Success profile) Settings
        , HH.div
            [ css "settings-page" ]
            [ HH.div
                [ css "container page" ]
                [ HH.div
                    [ css "row" ]
                    [ HH.div
                        [ css "col-md-6 offset-md-3 col-xs12" ]
                        html
                    ]
                ]
            ]
        ]
