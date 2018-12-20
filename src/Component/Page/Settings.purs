module Conduit.Component.Page.Settings where

import Prelude

import Conduit.Api.Request (AuthUser)
import Conduit.Component.HTML.Header (header)
import Conduit.Component.HTML.Utils (css)
import Conduit.Data.Avatar (Avatar)
import Conduit.Data.Email (Email)
import Conduit.Data.Route (Route(..))
import Conduit.Data.Username (Username)
import Conduit.Form.Field as Field
import Conduit.Form.Validation as V
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Formless as Formless
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

data Query a
  = HandleForm (F.Message' SettingsForm) a

type State =
  { authUser :: Maybe AuthUser }

type Input =
  { authUser :: Maybe AuthUser }

type ChildQuery m = F.Query' SettingsForm m
type ChildSlot = Unit

component 
  :: forall m
   . MonadAff m 
  => H.Component HH.HTML Query Input Void m
component = 
  H.parentComponent
    { initialState: identity
    , render
    , eval
    , receiver: const Nothing
    }
  where
  render :: State -> H.ParentHTML Query (ChildQuery m) ChildSlot m
  render { authUser } =
    container
      [ HH.h1
        [ css "text-xs-center"]
        [ HH.text "Your Settings" ]
        , HH.slot unit Formless.component 
            { initialInputs: F.mkInputFields formProxy
            , validators
            , render: renderFormless 
            } 
            (HE.input HandleForm)
      ]
    where
    container html =
      HH.div_
        [ header authUser Settings
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

  eval :: Query ~> H.ParentDSL State Query (ChildQuery m) Unit Void m
  eval = case _ of
    HandleForm msg a -> case msg of
      F.Submitted formOutputs -> pure a
      _ -> pure a

-----
-- Form

newtype SettingsForm r f = SettingsForm (r
  ( avatar :: f V.FormError String (Maybe Avatar)
  , username :: f V.FormError String (Maybe Username)
  , bio :: f Void String (Maybe String)
  , email :: f V.FormError String (Maybe Email)
  , password :: f V.FormError String (Maybe String)
  ))
derive instance newtypeSettingsForm :: Newtype (SettingsForm r f) _

formProxy :: F.FormProxy SettingsForm
formProxy = F.FormProxy

proxies :: F.SProxies SettingsForm
proxies = F.mkSProxies formProxy

validators :: forall m. Monad m => SettingsForm Record (F.Validation SettingsForm m)
validators = SettingsForm
  { avatar: V.toOptional V.avatarFormat
  , username: V.toOptional V.usernameFormat
  , bio: F.hoistFn_ pure
  , email: V.toOptional V.emailFormat
  , password: V.toOptional $ V.minLength 3 >>> V.maxLength 20
  }

renderFormless :: forall m. MonadAff m => F.State SettingsForm m -> F.HTML' SettingsForm m
renderFormless fstate =
  HH.form_
    [ HH.fieldset_
      [ profilePicture
      , username
      , bio
      , email
      , password
      ]
    , Field.submit "Update settings"
    ]
  where
  profilePicture =
    Field.input proxies.avatar fstate.form
      [ HP.placeholder "URL of profile picture", HP.type_ HP.InputText ]

  username = 
    Field.input proxies.username fstate.form
      [ HP.placeholder "Your name", HP.type_ HP.InputText ]

  bio = 
    HH.fieldset
      [ css "form-group" ]
      [ HH.textarea 
        [ css "form-control form-control-lg"
        , HP.placeholder "Short bio about you"
        , HP.value $ F.getInput proxies.bio fstate.form
        , HE.onValueInput $ HE.input $ F.setValidate proxies.bio
        ]
      ] 

  email = 
    Field.input proxies.email fstate.form
      [ HP.placeholder "Email", HP.type_ HP.InputEmail ]

  password = 
    Field.input proxies.password fstate.form
      [ HP.placeholder "Password", HP.type_ HP.InputPassword ]
    