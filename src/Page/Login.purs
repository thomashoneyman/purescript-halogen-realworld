-- | The login page supports a form users can submit to authenticate their session and gain access
-- | to the application.
module Conduit.Page.Login where

import Prelude

import Conduit.Capability.Navigate (class Navigate, navigate)
import Conduit.Capability.Resource.User (class ManageUser, loginUser)
import Conduit.Component.HTML.Header (header)
import Conduit.Component.HTML.Utils (css, safeHref)
import Conduit.Component.Utils (guardNoSession)
import Conduit.Data.Email (Email)
import Conduit.Data.Profile (Profile)
import Conduit.Data.Route (Route(..))
import Conduit.Form.Field (submit)
import Conduit.Form.Field as Field
import Conduit.Form.Validation as V
import Control.Monad.Reader (class MonadAsk)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect.Aff.Class (class MonadAff)
import Effect.Ref (Ref)
import Formless as F
import Formless as Formless
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

data Query a
  = Initialize a
  | HandleForm (F.Message' LoginForm) a

type ChildQuery m = F.Query' LoginForm m

component 
  :: forall m r
   . MonadAff m 
  => MonadAsk { currentUser :: Ref (Maybe Profile) | r } m
  => Navigate m
  => ManageUser m
  => H.Component HH.HTML Query Unit Void m
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
  eval :: Query ~> H.ParentDSL Unit Query (ChildQuery m) Unit Void m
  eval = case _ of
    Initialize a -> do
      guardNoSession
      pure a

    HandleForm msg a -> case msg of
      F.Submitted formOutputs -> do
        mbUser <- loginUser $ F.unwrapOutputFields formOutputs
        traverse_ (\_ -> navigate Home) mbUser
        pure a
      _ -> pure a

  render :: Unit -> H.ParentHTML Query (ChildQuery m) Unit m
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
      , HH.slot unit Formless.component 
          { initialInputs: F.mkInputFields formProxy
          , validators
          , render: renderFormless 
          } 
          (HE.input HandleForm)
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

-----
-- Form

-- | See the Formless tutorial to learn how to build your own forms: 
-- | https://github.com/thomashoneyman/purescript-halogen-formless

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
