module Conduit.Page.Login where

import Prelude

import Conduit.Api.Request (AuthUser)
import Conduit.Capability.Authenticate (class Authenticate, authenticate)
import Conduit.Capability.Navigate (class Navigate, navigate)
import Conduit.Component.HTML.Header (header)
import Conduit.Component.HTML.Utils (css, safeHref)
import Conduit.Data.Email (Email)
import Conduit.Data.Route (Route(..))
import Conduit.Form.Field (submit)
import Conduit.Form.Field as Field
import Conduit.Form.Validation as V
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (class Newtype)
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Formless as Formless
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

data Query a
  = Initialize a
  | HandleForm (F.Message' LoginForm) a

type State =
  { authUser :: Maybe AuthUser }

type Input =
  { authUser :: Maybe AuthUser }

type ChildQuery m = F.Query' LoginForm m
type ChildSlot = Unit

component 
  :: forall m
   . MonadAff m 
  => Navigate m
  => Authenticate m
  => H.Component HH.HTML Query Input Void m
component = 
  H.lifecycleParentComponent
    { initialState: identity
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just $ H.action Initialize
    , finalizer: Nothing
    }
  where
  render :: State -> H.ParentHTML Query (ChildQuery m) ChildSlot m
  render { authUser } =
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
        [ header authUser Login
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

  eval :: Query ~> H.ParentDSL State Query (ChildQuery m) Unit Void m
  eval = case _ of
    Initialize a -> do
      st <- H.get
      when (isJust st.authUser) (navigate Home)
      pure a

    HandleForm msg a -> case msg of
      F.Submitted formOutputs -> do
        eitherUser <- authenticate $ F.unwrapOutputFields formOutputs
        traverse_ (\_ -> navigate Home) eitherUser
        pure a
      _ -> pure a


-----
-- Form

newtype LoginForm r f = LoginForm (r
  ( email :: f V.FormError String Email
  , password :: f V.FormError String String
  ))

derive instance newtypeLoginForm :: Newtype (LoginForm r f) _

formProxy :: F.FormProxy LoginForm
formProxy = F.FormProxy

proxies :: F.SProxies LoginForm
proxies = F.mkSProxies formProxy

validators :: forall form m. Monad m => LoginForm Record (F.Validation form m)
validators = LoginForm
  { email: V.required >>> V.minLength 3 >>> V.emailFormat 
  , password: V.required >>> V.minLength 2 >>> V.maxLength 20
  }

renderFormless :: forall m. MonadAff m => F.State LoginForm m -> F.HTML' LoginForm m
renderFormless fstate =
  HH.form_
    [ HH.fieldset_
      [ email
      , password
      , submit "Log in"
      ]
    ]
  where
  email = 
    Field.input proxies.email fstate.form 
      [ HP.placeholder "Email", HP.type_ HP.InputEmail ]

  password = 
    Field.input proxies.password fstate.form 
      [ HP.placeholder "Password", HP.type_ HP.InputPassword ]
