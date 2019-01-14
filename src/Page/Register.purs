-- | The registration form allows new users to sign up to the Conduit service and authenticate
-- | their session.
module Conduit.Page.Register where

import Prelude

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
import Data.Foldable (for_)
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
  | HandleForm (F.Message' RegisterForm) a

type State =
  { currentUser :: Maybe Profile
  }

type ChildQuery m = F.Query' RegisterForm m

component 
  :: forall m r
   . MonadAff m 
  => MonadAsk { currentUser :: Ref (Maybe Profile) | r } m
  => ManageUser m
  => Navigate m
  => H.Component HH.HTML Query Unit Void m
component =
  H.lifecycleParentComponent
    { initialState: \_ -> { currentUser: Nothing }
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just $ H.action Initialize
    , finalizer: Nothing
    }
  where
  eval :: Query ~> H.ParentDSL State Query (ChildQuery m) Unit Void m
  eval = case _ of
    Initialize a -> do
      guardNoSession 
      pure a

    HandleForm msg a -> case msg of
      F.Submitted formOutputs -> do 
        mbUser <- registerUser $ F.unwrapOutputFields formOutputs
        for_ mbUser (\_ -> navigate Home)
        pure a
      _ -> pure a

  render :: State -> H.ParentHTML Query (ChildQuery m) Unit m
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


-----
-- Form

-- | See the Formless tutorial to learn how to build your own forms: 
-- | https://github.com/thomashoneyman/purescript-halogen-formless

newtype RegisterForm r f = RegisterForm (r
  ( username :: f V.FormError String Username
  , email :: f V.FormError String Email
  , password :: f V.FormError String String
  ))

derive instance newtypeRegisterForm :: Newtype (RegisterForm r f) _

formProxy :: F.FormProxy RegisterForm
formProxy = F.FormProxy

proxies :: F.SProxies RegisterForm
proxies = F.mkSProxies formProxy

validators :: forall m. Monad m => RegisterForm Record (F.Validation RegisterForm m)
validators = RegisterForm
  { username: V.required >>> V.usernameFormat
  , email: V.required >>> V.minLength 3 >>> V.emailFormat
  , password: V.required >>> V.minLength 8 >>> V.maxLength 20
  }

renderFormless :: forall m. MonadAff m => F.State RegisterForm m -> F.HTML' RegisterForm m
renderFormless fstate =
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
    Field.input proxies.username fstate.form 
      [ HP.placeholder "Username", HP.type_ HP.InputText ]

  email = 
    Field.input proxies.email fstate.form 
      [ HP.placeholder "Email", HP.type_ HP.InputEmail ] 

  password = 
    Field.input proxies.password fstate.form 
      [ HP.placeholder "Password" , HP.type_ HP.InputPassword ]
