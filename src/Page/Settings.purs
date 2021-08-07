-- | The settings page lets users change data about their account, like their email or password,
-- | as well as their publicly-viewable profile information.
module Conduit.Page.Settings where

import Prelude

import Conduit.Capability.Navigate (class Navigate, logout)
import Conduit.Capability.Resource.User (class ManageUser, UpdateProfileFields, getCurrentUser, updateUser)
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
import Conduit.Form.Validation as V
import Data.Lens (preview)
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Newtype (class Newtype, unwrap)
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..), _Success, fromMaybe)
import Type.Proxy (Proxy(..))
import Web.Event.Event as Event

-- | See the Formless tutorial to learn how to build your own forms:
-- | https://github.com/thomashoneyman/purescript-halogen-formless

newtype SettingsForm (r :: Row Type -> Type) f = SettingsForm
  ( r
      ( image :: f V.FormError String (Maybe Avatar)
      , username :: f V.FormError String Username
      , bio :: f Void String (Maybe String)
      , email :: f V.FormError String Email
      , password :: f V.FormError String (Maybe String)
      )
  )

derive instance newtypeSettingsForm :: Newtype (SettingsForm r f) _

data Action
  = Initialize
  | HandleForm UpdateProfileFields
  | LogUserOut

type State =
  { profile :: RemoteData String ProfileWithEmail }

component
  :: forall q o m
   . MonadAff m
  => Navigate m
  => ManageUser m
  => H.Component q Unit o m
component = H.mkComponent
  { initialState: \_ -> { profile: NotAsked }
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
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
          let
            newInputs = F.wrapInputFields
              { image: Maybe.fromMaybe "" $ Avatar.toString <$> profile.image
              , username: Username.toString profile.username
              , bio: Maybe.fromMaybe "" profile.bio
              , email: unwrap profile.email
              , password: ""
              }
          void $ H.query F._formless unit $ F.asQuery $ F.loadForm newInputs

    HandleForm fields -> do
      updateUser fields
      mbProfileWithEmail <- getCurrentUser
      H.modify_ _ { profile = fromMaybe mbProfileWithEmail }

    LogUserOut -> logout

  render { profile } =
    container
      [ HH.h1
          [ css "text-xs-center" ]
          [ HH.text "Your Settings" ]
      , HH.slot F._formless unit formComponent unit HandleForm
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

data FormAction = Submit Event.Event

formComponent
  :: forall formQuery formSlots formInput m
   . MonadAff m
  => F.Component SettingsForm formQuery formSlots formInput UpdateProfileFields m
formComponent = F.component formInput $ F.defaultSpec
  { render = renderForm
  , handleEvent = handleEvent
  , handleAction = handleAction
  }
  where
  formInput :: formInput -> F.Input' SettingsForm m
  formInput _ =
    { validators: SettingsForm
        { image: V.toOptional V.avatarFormat
        , username: V.required >>> V.minLength 3 >>> V.maxLength 20 >>> V.usernameFormat
        , bio: F.hoistFn_ pure
        , email: V.required >>> V.minLength 3 >>> V.maxLength 50 >>> V.emailFormat
        , password: V.toOptional $ V.minLength 3 >>> V.maxLength 20
        }
    , initialInputs: Nothing
    }

  handleEvent = F.raiseResult

  handleAction = case _ of
    Submit event -> do
      H.liftEffect $ Event.preventDefault event
      eval F.submit
    where
    eval act = F.handleAction handleAction handleEvent act

  renderForm { form } =
    HH.form
      [ HE.onSubmit \ev -> F.injAction $ Submit ev ]
      [ HH.fieldset_
          [ image
          , username
          , bio
          , email
          , password
          , Field.submit "Update settings"
          ]
      ]
    where
    image =
      Field.input (Proxy :: Proxy "image") form
        [ HP.placeholder "URL of profile picture", HP.type_ HP.InputText ]

    username =
      Field.input (Proxy :: Proxy "username") form
        [ HP.placeholder "Your name", HP.type_ HP.InputText ]

    bio =
      HH.fieldset
        [ css "form-group" ]
        [ HH.textarea
            [ css "form-control form-control-lg"
            , HP.placeholder "Short bio about you"
            , HP.rows 8
            , HP.value $ F.getInput (Proxy :: Proxy "bio") form
            , HE.onValueInput $ F.setValidate (Proxy :: Proxy "bio")
            ]
        ]

    email =
      Field.input (Proxy :: Proxy "email") form
        [ HP.placeholder "Email", HP.type_ HP.InputEmail ]

    password =
      Field.input (Proxy :: Proxy "password") form
        [ HP.placeholder "Password", HP.type_ HP.InputPassword ]
