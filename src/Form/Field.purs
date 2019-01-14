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
import Conduit.Form.Validation (errorToString)
import Conduit.Form.Validation as V
import DOM.HTML.Indexed (HTMLinput)
import Data.Newtype (class Newtype)
import Data.Symbol (class IsSymbol, SProxy)
import Data.Variant (Variant)
import Formless as F
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Row as Row

-- | Formless (the form library for Halogen) supports a submit event which will attempt to validate
-- | and return the successfully-parsed fields. We can create a small helper function that creates
-- | a submit button with customizable text and the submit event triggered by a click. Since all 
-- | submit buttons in the Conduit application look the same, we can jus use this throughout the app.
submit :: forall pq cq cs form m. String -> F.HTML pq cq cs form m
submit buttonText =
  HH.button
    [ css "btn btn-lg btn-primary pull-xs-right"
    , HE.onClick $ HE.input_ F.submit 
    ]
    [ HH.text buttonText ]

-- | This helper function creates an input field hooked up with Formless, including styles,
-- | events, error handling, and more. The unction ensures at compile-time that the field we 
-- | want actually exists in the form, that the input, error, and output types of the field are 
-- | compatible, that the only properties you attempt to set on the HTML are actual valid <input> 
-- | properties, and more.
-- |
-- | Let's deconstruct the type.
-- |
-- | First, the `IsSymbol` constraint requires that our first argument, `sym`, is a type-level
-- | string. You've seen these all over the place -- record labels are one example. We'll use 
-- | this any time we need to talk about a value existing at a particular key in a record or
-- | a variant.
-- |
-- | Next, the two `Newtype` constraints require that you can use the `unwrap` function to 
-- | transform the first type into the second type. In other words, the first type has to have
-- | a `Newtype` instance. This is how we'll unpack our self-defined Formless form type into
-- | either a raw record or variant we can work with.
-- |
-- | Next, the two `Cons` constraints require that there exists a value of the type given in
-- | the second argument at the label `sym` in the record or variant given in the last argument.
-- | For instance, we require that there's a field with an error type `FormError` and an input
-- | type `String` at the label `sym` in the row `fields`. In short, we require at compile-time
-- | that an input field of the correct type exists in our form state at the key we provided as  
-- | the function's first argument.
input 
  :: forall pq cq cs form m fields inputs sym o t0 t1
   . IsSymbol sym
  => Newtype (form Record F.FormField) { | fields }
  => Newtype (form Variant F.InputFunction) (Variant inputs)
  => Row.Cons sym (F.FormField V.FormError String o) t0 fields
  => Row.Cons sym (F.InputFunction V.FormError String o) t1 inputs
  => SProxy sym
  -> form Record F.FormField
  -> Array (HH.IProp HTMLinput (F.Query pq cq cs form m Unit))
  -> F.HTML pq cq cs form m
input sym form props =
  HH.fieldset
    [ css "form-group" ]
    [ HH.input 
      ( append
          [ css "form-control form-control-lg"
          , HP.value $ F.getInput sym form
          , HE.onValueInput $ HE.input $ F.setValidate sym
          ]
          props
      )
    , maybeElem (F.getError sym form) \err ->
        HH.div
          [ css "error-messages" ]
          [ HH.text $ errorToString err ]
    ] 
