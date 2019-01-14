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
import Conduit.Form.Validation as V
import Data.Lens (preview)
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Newtype (class Newtype, unwrap)
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Formless as Formless
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..), _Success, fromMaybe, toMaybe)

data Query a
  = Initialize a
  | HandleForm (F.Message' SettingsForm) a
  | LogUserOut a

type State =
  { profile :: RemoteData String ProfileWithEmail 
  }

type ChildQuery m = F.Query' SettingsForm m

component 
  :: forall m
   . MonadAff m 
  => Navigate m
  => ManageUser m
  => H.Component HH.HTML Query Unit Void m
component = 
  H.lifecycleParentComponent
    { initialState: \_ -> { profile: NotAsked }
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
          void $ H.query unit $ F.loadForm_ newInputs 

      pure a

    HandleForm msg a -> case msg of
      F.Submitted formOutputs -> do
        updateUser $ F.unwrapOutputFields formOutputs
        mbProfileWithEmail <- getCurrentUser
        H.modify_ _ { profile = fromMaybe mbProfileWithEmail }
        pure a
      _ -> pure a
    
    LogUserOut a -> logout *> pure a

  render :: State -> H.ParentHTML Query (ChildQuery m) Unit m
  render { profile } =
    container
      [ HH.h1
        [ css "text-xs-center"]
        [ HH.text "Your Settings" ]
      , HH.slot unit Formless.component 
          { initialInputs: F.mkInputFields formProxy
          , validators
          , render: renderFormless (toMaybe profile)
          } 
          (HE.input HandleForm)
      , HH.hr_  
      , HH.button
        [ css "btn btn-outline-danger" 
        , HE.onClick $ HE.input_ LogUserOut 
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

-----
-- Form

-- | See the Formless tutorial to learn how to build your own forms: 
-- | https://github.com/thomashoneyman/purescript-halogen-formless

newtype SettingsForm r f = SettingsForm (r
  ( image :: f V.FormError String (Maybe Avatar)
  , username :: f V.FormError String Username
  , bio :: f Void String (Maybe String)
  , email :: f V.FormError String Email
  , password :: f V.FormError String (Maybe String)
  ))
derive instance newtypeSettingsForm :: Newtype (SettingsForm r f) _

formProxy :: F.FormProxy SettingsForm
formProxy = F.FormProxy

proxies :: F.SProxies SettingsForm
proxies = F.mkSProxies formProxy

validators :: forall m. Monad m => SettingsForm Record (F.Validation SettingsForm m)
validators = SettingsForm
  { image: V.toOptional V.avatarFormat
  , username: V.required >>> V.minLength 3 >>> V.maxLength 20 >>> V.usernameFormat
  , bio: F.hoistFn_ pure
  , email: V.required >>> V.minLength 3 >>> V.maxLength 50 >>> V.emailFormat
  , password: V.toOptional $ V.minLength 3 >>> V.maxLength 20
  }

renderFormless 
  :: forall m
   . MonadAff m 
  => Maybe ProfileWithEmail 
  -> F.State SettingsForm m 
  -> F.HTML' SettingsForm m
renderFormless mbProf fstate =
  HH.form_
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
    Field.input proxies.image fstate.form
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
        , HP.rows 8
        , HE.onValueInput $ HE.input $ F.setValidate proxies.bio
        ]
      ] 

  email = 
    Field.input proxies.email fstate.form
      [ HP.placeholder "Email", HP.type_ HP.InputEmail ]

  password = 
    Field.input proxies.password fstate.form
      [ HP.placeholder "Password", HP.type_ HP.InputPassword ]
    