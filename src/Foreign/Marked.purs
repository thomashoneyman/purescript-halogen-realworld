module Conduit.Foreign.Marked where

import Prelude

import Data.Newtype (class Newtype)

newtype RawHTML = RawHTML String

derive instance newtypeRawHTML :: Newtype RawHTML _

-- | Will always return a string, even if malformed. Buyer beware! 
foreign import markedImpl :: String -> String

marked :: String -> RawHTML
marked = RawHTML <<< markedImpl