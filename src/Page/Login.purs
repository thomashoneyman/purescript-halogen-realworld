-- | The login page supports a form users can submit to authenticate their session and gain access
-- | to the application.
module Conduit.Page.Login where

import Prelude

import Conduit.Api.Request (LoginFields)
import Conduit.Capability.Navigate (class Navigate, navigate)
import Conduit.Capability.Resource.User (class ManageUser, loginUser)
import Conduit.Component.HTML.Header (header)
import Conduit.Component.HTML.Utils (css, safeHref)
import Conduit.Data.Email (Email)
import Conduit.Data.Route (Route(..))
import Conduit.Form.Field (submit)
import Conduit.Form.Field as Field
import Conduit.Form.Validation as V
import Data.Const (Const)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

-- | See the Formless tutorial to learn how to build your own forms: 
-- | https://github.com/thomashoneyman/purescript-halogen-formless

newtype LoginForm r f = LoginForm (r
  ( email :: f V.FormError String Email
  , password :: f V.FormError String String
  ))

derive instance newtypeLoginForm :: Newtype (LoginForm r f) _

data Action
  = HandleLoginForm LoginFields

-- Should this component redirect to home after login or not? If the login page is loaded
-- at the login route, then yes; if not, then it is guarding another route and should not.
type State = 
  { redirect :: Boolean }

type Input =
  { redirect :: Boolean }

type ChildSlots = 
  ( formless :: F.Slot LoginForm (Const Void) () LoginFields Unit )

component 
  :: forall m
   . MonadAff m 
  => Navigate m
  => ManageUser m
  => H.Component HH.HTML (Const Void) Input Void m
component = H.mkComponent
  { initialState: identity
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }
  where
  handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
  handleAction = case _ of
    HandleLoginForm fields -> do
      st <- H.get
      loginUser fields >>= traverse_ (\_ -> when st.redirect $ navigate Home)

  render :: State -> H.ComponentHTML Action ChildSlots m
  render _ =
    container
      [ HH.h1
          [ css "text-xs-center"]
          [ HH.text "Sign In" ]
      , HH.p
          [ css "text-xs-center" ]
          [ HH.a 
              [ safeHref Register ]
              [ HH.text "Need an account?" ]
        ]
      , HH.slot F._formless unit (F.component formSpec) formInput (Just <<< HandleLoginForm) 
      ]
    where
    container html =
      HH.div
        [ css "auth-page" ]
        [ header Nothing Login
        , HH.div
            [ css "container page" ]
            [ HH.div
                [ css "row" ]
                [ HH.div
                    [ css "col-md-6 offset-md-3 col-xs12" ]
                    html
                ]
            ]
        ]
    
    formInput :: F.Input' LoginForm m
    formInput =
      { validators: LoginForm
          { email: V.required >>> V.minLength 3 >>> V.emailFormat 
          , password: V.required >>> V.minLength 2 >>> V.maxLength 20
          }
      , initialInputs: Nothing
      }

    formSpec :: F.Spec' LoginForm LoginFields m
    formSpec = F.defaultSpec
      { render = renderLogin 
      , handleMessage = handleMessage
      }
      where
      handleMessage = case _ of
        F.Submitted outputs -> H.raise (F.unwrapOutputFields outputs)
        _ -> pure unit

      proxies = F.mkSProxies $ F.FormProxy :: _ LoginForm

      renderLogin { form } =
        HH.form_
          [ HH.fieldset_
            [ Field.input proxies.email form 
                [ HP.placeholder "Email"
                , HP.type_ HP.InputEmail 
                ]
            , Field.input proxies.password form
                [ HP.placeholder "Password"
                , HP.type_ HP.InputPassword 
                ]
            , submit "Log in"
            ]
          ]
