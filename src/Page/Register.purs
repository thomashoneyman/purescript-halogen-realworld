-- | The registration form allows new users to sign up to the Conduit service and authenticate
-- | their session.
module Conduit.Page.Register where

import Prelude

import Conduit.Api.Request (RegisterFields)
import Conduit.Capability.Navigate (class Navigate, navigate)
import Conduit.Capability.Resource.User (class ManageUser, registerUser)
import Conduit.Component.HTML.Header (header)
import Conduit.Component.HTML.Utils (css, safeHref)
import Conduit.Component.Utils (guardNoSession)
import Conduit.Data.Email (Email)
import Conduit.Data.Profile (Profile)
import Conduit.Data.Route (Route(..))
import Conduit.Data.Username (Username)
import Conduit.Form.Field as Field
import Conduit.Form.Validation as V
import Control.Monad.Reader (class MonadAsk)
import Data.Const (Const)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect.Aff.Class (class MonadAff)
import Effect.Ref (Ref)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

-- | See the Formless tutorial to learn how to build your own forms: 
-- | https://github.com/thomashoneyman/purescript-halogen-formless

newtype RegisterForm r f = RegisterForm (r
  ( username :: f V.FormError String Username
  , email :: f V.FormError String Email
  , password :: f V.FormError String String
  ))

derive instance newtypeRegisterForm :: Newtype (RegisterForm r f) _

data Action
  = Initialize
  | HandleRegisterForm RegisterFields

type State =
  { currentUser :: Maybe Profile
  }

type ChildSlots = 
  ( formless :: F.Slot RegisterForm (Const Void) () RegisterFields Unit )

component 
  :: forall m r
   . MonadAff m 
  => MonadAsk { currentUser :: Ref (Maybe Profile) | r } m
  => ManageUser m
  => Navigate m
  => H.Component HH.HTML (Const Void) Unit Void m
component = H.mkComponent
  { initialState: \_ -> { currentUser: Nothing }
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction 
      , initialize = Just Initialize
      }
  }
  where
  handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
  handleAction = case _ of
    Initialize ->
      guardNoSession 

    HandleRegisterForm fields ->
      registerUser fields >>= traverse_ (\_ -> navigate Home)

  render :: State -> H.ComponentHTML Action ChildSlots m
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
      , HH.slot F._formless unit (F.component formSpec) formInput (Just <<< HandleRegisterForm)
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

    formInput :: F.Input' RegisterForm m
    formInput =
      { validators: RegisterForm
          { username: V.required >>> V.usernameFormat
          , email: V.required >>> V.minLength 3 >>> V.emailFormat
          , password: V.required >>> V.minLength 8 >>> V.maxLength 20
          }
      , initialInputs: Nothing
      }
    
    formSpec :: F.Spec' RegisterForm RegisterFields m
    formSpec = F.defaultSpec
      { render = renderForm 
      , handleMessage = handleMessage 
      }
      where
      handleMessage = case _ of
        F.Submitted outputs -> H.raise (F.unwrapOutputFields outputs)
        _ -> pure unit

      proxies = F.mkSProxies $ F.FormProxy :: _ RegisterForm

      renderForm { form } =
        HH.form_
          [ HH.fieldset_
            [ username
            , email
            , password
            ]
          , Field.submit "Sign up"
          ]
        where
        username = 
          Field.input proxies.username form 
            [ HP.placeholder "Username", HP.type_ HP.InputText ]

        email = 
          Field.input proxies.email form 
            [ HP.placeholder "Email", HP.type_ HP.InputEmail ] 

        password = 
          Field.input proxies.password form 
            [ HP.placeholder "Password" , HP.type_ HP.InputPassword ]
