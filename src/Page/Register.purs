-- | The registration form allows new users to sign up to the Conduit service and authenticate
-- | their session.
module Conduit.Page.Register where

import Prelude

import Conduit.Capability.Navigate (class Navigate, navigate)
import Conduit.Capability.Resource.User (class ManageUser, registerUser)
import Conduit.Component.HTML.Header (header)
import Conduit.Component.HTML.Utils (css, safeHref)
import Conduit.Data.Email (Email)
import Conduit.Data.Route (Route(..))
import Conduit.Data.Username (Username)
import Conduit.Form.Field as Field
import Conduit.Form.Validation (FormError)
import Conduit.Form.Validation as V
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

-- | See the Formless tutorial to learn how to build your own forms:
-- | https://github.com/thomashoneyman/purescript-halogen-formless

type Form :: (Type -> Type -> Type -> Type) -> Row Type
type Form f =
  ( username :: f String FormError Username
  , email :: f String FormError Email
  , password :: f String FormError String
  )

type FormContext = F.FormContext (Form F.FieldState) (Form (F.FieldAction Action)) Unit Action
type FormlessAction = F.FormlessAction (Form F.FieldState)

data Action
  = Receive FormContext
  | Eval FormlessAction

component
  :: forall query output m
   . MonadAff m
  => ManageUser m
  => Navigate m
  => H.Component query Unit output m
component = F.formless { liftAction: Eval } mempty $ H.mkComponent
  { initialState: \context -> context
  , render
  , eval: H.mkEval $ H.defaultEval
      { receive = Just <<< Receive
      , handleAction = handleAction
      , handleQuery = handleQuery
      }
  }
  where
  handleAction :: Action -> H.HalogenM _ _ _ _ _ Unit
  handleAction = case _ of
    Receive context -> H.put context
    Eval action -> F.eval action

  handleQuery :: forall a. F.FormQuery _ _ _ _ a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery = do
    let
      onSubmit = registerUser >=> traverse_ (\_ -> navigate Home)
      validation =
        { username: V.required >=> V.usernameFormat
        , email: V.required >=> V.minLength 3 >=> V.emailFormat
        , password: V.required >=> V.minLength 8 >=> V.maxLength 20
        }

    F.handleSubmitValidate onSubmit F.validate validation

  render :: FormContext -> H.ComponentHTML Action () m
  render { formActions, fields, actions } =
    container
      [ HH.h1
          [ css "text-xs-center" ]
          [ HH.text "Sign Up" ]
      , HH.p
          [ css "text-xs-center" ]
          [ HH.a
              [ safeHref Login ]
              [ HH.text "Already have an account?" ]
          ]
      , HH.form
          [ HE.onSubmit formActions.handleSubmit ]
          [ HH.fieldset_
              [ Field.textInput
                  { state: fields.username, action: actions.username }
                  [ HP.placeholder "Username" ]
              , Field.textInput
                  { state: fields.email, action: actions.email }
                  [ HP.placeholder "Email"
                  , HP.type_ HP.InputEmail
                  ]
              , Field.textInput
                  { state: fields.password, action: actions.password }
                  [ HP.placeholder "Password "
                  , HP.type_ HP.InputPassword
                  ]
              , Field.submitButton "Sign up"
              ]
          ]
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
