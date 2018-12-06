module Component.Page.Settings where

import Prelude

import Component.HTML.Utils (css)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Formless as Formless
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

data Query a =
  HandleForm (F.Message' ContactForm) a

type ChildQuery m = F.Query' ContactForm m
type ChildSlot = Unit

component :: forall m. MonadAff m => H.Component HH.HTML Query Unit Void m
component = H.parentComponent
  { initialState: const unit
  , render
  , eval
  , receiver: const Nothing
  }
  where

  render :: Unit -> H.ParentHTML Query (ChildQuery m) ChildSlot m
  render _ =
    container
        [ HH.h1
        [ css "text-xs-center"]
        [ HH.text "Your Settings" ]
        , HH.slot unit Formless.component 
            { initialInputs
            , validators: F.noValidation initialInputs
            , render: renderFormless 
            } 
            (HE.input HandleForm)
        ]
    where
    container html =
      HH.div
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

  eval :: Query ~> H.ParentDSL Unit Query (ChildQuery m) Unit Void m
  eval (HandleForm (F.Submitted formOutputs) a) = pure a
  eval (HandleForm _ a) = pure a

-----
-- Form

newtype ContactForm r f = ContactForm (r
  ( avatar :: f Void String String
  , name :: f Void String String
  , bio :: f Void String String
  , email :: f Void String String
  , password :: f Void String String
  ))
derive instance newtypeContactForm :: Newtype (ContactForm r f) _

formProxy :: F.FormProxy ContactForm
formProxy = F.FormProxy

prx :: F.SProxies ContactForm
prx = F.mkSProxies formProxy

initialInputs :: ContactForm Record F.InputField
initialInputs = F.mkInputFields formProxy

renderFormless :: forall m. MonadAff m => F.State ContactForm m -> F.HTML' ContactForm m
renderFormless fstate =
  HH.form_
    [ HH.fieldset_
      [ profilePicture
      ]
    ]
  where
    input placeholder val onInput type_ =
      HH.fieldset
        [ css "form-group" ]
        [ HH.input 
          [ css "form-control-lg"
          , HP.type_ type_
          , HP.placeholder placeholder 
          , HP.value val
          , HE.onValueInput onInput
          ]
        ]

    profilePicture = 
      input 
        "URL of profile picture" 
        (F.getInput prx.avatar fstate.form)
        (HE.input $ F.setValidate prx.avatar)
        HP.InputText

    name = 
      input 
        "Your Name" 
        (F.getInput prx.name fstate.form)
        (HE.input $ F.setValidate prx.name)
        HP.InputText

    bio = 
      input 
        "Short bio about you" 
        (F.getInput prx.bio fstate.form)
        (HE.input $ F.setValidate prx.bio)
        HP.InputText

    email = 
      input 
        "Email" 
        (F.getInput prx.email fstate.form)
        (HE.input $ F.setValidate prx.email)
        HP.InputText

    password = 
      input 
        "Password" 
        (F.getInput prx.password fstate.form)
        (HE.input $ F.setValidate prx.password)
        HP.InputPassword
    
    submit =
      HH.button
        [ css "btn btn-lg btn-primary pull-xs-right" 
        , HE.onClick $ HE.input_ F.submit
        ]
        [ HH.text "Update Settings" ]
