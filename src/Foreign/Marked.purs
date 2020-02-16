-- | This module leverages the PureScript foreign function interface (FFI) to import a popular NPM
-- | library, Marked, to parse a markdown string into HTML. The FFI should be used sparingly, as
-- | JavaScript functions can exhibit surprising behavior (such as returning an unexpected `null`).
-- | The compiler puts its full trust in you! Foreign functions are not type checked and can be a
-- | source of subtle bugs in your program.
-- |
-- | Despite these caveats, the FFI is an indispensible tool. The most common use is to import useful
-- | libraries that have not been implemented in PureScript yet.In our case, the `halogen-markdown`
-- | library has not been updated for the current version of the compiler and is designed for a non-
-- | standard flavor of markdown. Rather than implement something myself, I've deferred to the popular
-- | Marked library. PureScript developers also use the FFI for bare-metal code that needs to run as
-- | fast as possible. For example, large portions of the `purescript-aff` library rely on hand-tuned
-- | JavaScript for this reason.
-- |
-- | You can learn more about the PureScript foreign function interface (FFI) in PureScript by Example:
-- | https://leanpub.com/purescript/read#leanpub-auto-the-foreign-function-interface
module Conduit.Foreign.Marked
  ( RawHTML
  , marked
  ) where

-- | The `Marked` library transforms an input string (which should be markdown) into an output HTML
-- | string, even if it failed to parse the input markdown. I'd like to know at glance that I'm
-- | using a string of HTML, so we'll use a newtype to track that information in the type system.
newtype RawHTML = RawHTML String

-- | The `markedImpl` function is a native JavaScript function we're importing into PureScript. The
-- | compiler will not check this type.
-- |
-- | It's best to use native JavaScript types when you use the FFI, and then translate them into
-- | PureScript types. For example, we foreign import `markedImpl`, but export a different function
-- | which makes better use of the type system.
foreign import markedImpl :: String -> String

-- | For such a simple function there's little conversion needed from JavaScript to PureScript. All
-- | that we'll do is wrap the result string in our `RawHTML` newtype. More complex foreign imports
-- | warrant more sophisticated interoperability.
marked :: String -> RawHTML
marked str = RawHTML (markedImpl str)
