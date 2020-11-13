-- | The registration form allows new users to sign up to the Conduit service and authenticate
-- | their session.
module Conduit.Page.Register where

import Prelude

import Conduit.Api.Request (RegisterFields)
import Conduit.Capability.Navigate (class Navigate, navigate)
import Conduit.Capability.Resource.User (class ManageUser, registerUser)
import Conduit.Component.HTML.Header (header)
import Conduit.Component.HTML.Utils (css, safeHref)
import Conduit.Data.Email (Email)
import Conduit.Data.Route (Route(..))
import Conduit.Data.Username (Username)
import Conduit.Form.Field as Field
import Conduit.Form.Validation as V
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event as Event

-- | See the Formless tutorial to learn how to build your own forms:
-- | https://github.com/thomashoneyman/purescript-halogen-formless

newtype RegisterForm r f = RegisterForm (r
  ( username :: f V.FormError String Username
  , email :: f V.FormError String Email
  , password :: f V.FormError String String
  ))

derive instance newtypeRegisterForm :: Newtype (RegisterForm r f) _

data Action
  = HandleRegisterForm RegisterFields

component
  :: forall q o m
   . MonadAff m
  => ManageUser m
  => Navigate m
  => H.Component HH.HTML q Unit o m
component = H.mkComponent
  { initialState: const unit
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }
  where
  handleAction = case _ of
    HandleRegisterForm fields ->
      registerUser fields >>= traverse_ (\_ -> navigate Home)

  render _ =
    container
      [ HH.h1
          [ css "text-xs-center"]
          [ HH.text "Sign Up" ]
      , HH.p
          [ css "text-xs-center" ]
          [ HH.a
              [ safeHref Login ]
              [ HH.text "Already have an account?" ]
        ]
      , HH.slot F._formless unit formComponent unit (Just <<< HandleRegisterForm)
      ]
    where
    container html =
      HH.div_
        [ header Nothing Register
        , HH.div
            [ css "auth-page" ]
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
  => F.Component RegisterForm formQuery formSlots formInput RegisterFields m
formComponent = F.component formInput $ F.defaultSpec
  { render = renderForm
  , handleEvent = handleEvent
  , handleAction = handleAction
  }
  where
  formInput :: formInput -> F.Input' RegisterForm m
  formInput _ =
    { validators: RegisterForm
        { username: V.required >>> V.usernameFormat
        , email: V.required >>> V.minLength 3 >>> V.emailFormat
        , password: V.required >>> V.minLength 8 >>> V.maxLength 20
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
      [ HE.onSubmit \ev -> Just $ F.injAction $ Submit ev ]
      [ HH.fieldset_
          [ username
          , email
          , password
          ]
      , Field.submit "Sign up"
      ]
    where
    proxies = F.mkSProxies (F.FormProxy :: _ RegisterForm)

    username =
      Field.input proxies.username form
        [ HP.placeholder "Username", HP.type_ HP.InputText ]

    email =
      Field.input proxies.email form
        [ HP.placeholder "Email", HP.type_ HP.InputEmail ]

    password =
      Field.input proxies.password form
        [ HP.placeholder "Password" , HP.type_ HP.InputPassword ]
