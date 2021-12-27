-- | This module provides small utilities for building form fields with Formless. They're not
-- | necessary to use the library, but help alleviate a little boilerplate around handling
-- | inputs. To read more about how to use the Formless library, see:
-- | https://github.com/thomashoneyman/purescript-halogen-formless
-- |
-- | In a framework like React, little bundles of functionality like this might be individual
-- | components. In Halogen, they're simple pure functions which produce HTML.
module Conduit.Form.Field where

import Prelude

import Conduit.Component.HTML.Utils (css, maybeElem)
import Conduit.Form.Validation (FormError, errorToString)
import DOM.HTML.Indexed (HTMLinput, HTMLtextarea)
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

-- | This helper type can be used to define form fields that take a `String`
-- | input, produce validation errors of type `FormError`, and parse to the
-- | provided output type. For example, this form field:
-- |
-- | ```purs
-- | type Form f = ( name :: f String FormError Username )
-- | ```
-- |
-- | could be rewritten using this helper type:
-- |
-- | ```purs
-- | type Form f = ( name :: StringField f Username )
-- | ```
type StringField :: (Type -> Type -> Type -> Type) -> Type -> Type
type StringField f output = f String FormError output

-- | This small helper function that creates a submit button with customizable
-- | text. Since all submit buttons in the Conduit application look the same,
-- | we can just use this throughout the app.
submitButton :: forall i p. String -> HH.HTML i p
submitButton label =
  HH.input
    [ css "btn btn-lg btn-primary pull-xs-right"
    , HP.type_ HP.InputSubmit
    , HP.value label
    ]

type TextInput action output =
  { state :: F.FieldState String FormError output
  , action :: F.FieldAction action String FormError output
  }

-- | Our application ought to define a set of reusable, styled form controls that
-- | are compatible with Formless. This helper function constructs an input field
-- | with styles, events, error handling, and more, given a Formless field.
textInput
  :: forall output action slots m
   . TextInput action output
  -> Array (HP.IProp HTMLinput action)
  -> H.ComponentHTML action slots m
textInput { state, action } props =
  HH.fieldset
    [ css "form-group" ]
    [ HH.input
        ( append
            [ css "form-control form-control-lg"
            , HP.value state.value
            , HE.onValueInput action.handleChange
            , HE.onBlur action.handleBlur
            ]
            props
        )
    , maybeElem (state.result >>= either pure (const Nothing)) \err ->
        HH.div
          [ css "error-messages" ]
          [ HH.text $ errorToString err ]
    ]

textarea
  :: forall output action slots m
   . TextInput action output
  -> Array (HP.IProp HTMLtextarea action)
  -> H.ComponentHTML action slots m
textarea { state, action } props =
  HH.fieldset
    [ css "form-group" ]
    [ HH.textarea
        ( append
            [ css "form-control form-control-lg"
            , HP.rows 8
            , HP.value state.value
            , HE.onValueInput action.handleChange
            , HE.onBlur action.handleBlur
            ]
            props
        )
    ]
