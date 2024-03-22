module Data.Slug
  ( Slug
  , generate
  , parse
  , toString
  , truncate
  ) where

import Prelude

import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..), decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Array as Array
import Data.CodePoint.Unicode (isLatin1)
import Data.Either (note)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.CodePoints (codePointFromChar, fromCodePointArray, toCodePointArray)
import Data.String.Pattern (Pattern(..), Replacement(..))

-- | A `Slug` represents a string value which is guaranteed to have the
-- | following qualities:
-- |
-- | - it is not empty
-- | - it consists of groups of characters separated by `-` dashes,
-- |   where the slug cannot begin or end with a dash, and there can
-- |   never be two dashes in a row.
-- |
-- | Example: `Slug "this-is-an-article-slug"`
newtype Slug = Slug String

derive newtype instance Eq Slug
derive newtype instance Ord Slug
derive newtype instance Semigroup Slug

instance Show Slug where
  show (Slug str) = "(Slug " <> str <> ")"

instance EncodeJson Slug where
  encodeJson (Slug s) = encodeJson s

instance DecodeJson Slug where
  decodeJson = note (TypeMismatch "Slug") <<< parse <=< decodeJson

-- | Create a `Slug` from a string. This will transform the input string
-- | to be a valid slug (if it is possible to do so) by separating words
-- | with `-` dashes, ensuring the string does not begin or end with a
-- | dash, and ensuring there are never two dashes in a row.
-- |
-- | Slugs are usually created for article titles and other resources
-- | which need a human-readable resource name in a URL.
-- |
-- | ```purescript
-- | > Slug.generate "My article title!"
-- | > Just (Slug "My-article-title")
-- |
-- | > Slug.generate "¬¬¬{}¬¬¬"
-- | > Nothing
-- | ```
generate :: String -> Maybe Slug
generate s = do
  let arr = words $ onlyLatin1 $ stripApostrophes s
  if Array.null arr then
    Nothing
  else
    Just $ Slug $ String.joinWith "-" arr
  where
  -- Strip apostrophes to avoid unnecessary word breaks
  stripApostrophes = String.replaceAll (Pattern "'") (Replacement "")

  -- Replace non-Latin 1 characters with spaces to be stripped later.
  onlyLatin1 =
    fromCodePointArray
      <<< map (\x -> if isLatin1 x then x else codePointFromChar ' ')
      <<< toCodePointArray

  -- Split on whitespace
  words = Array.filter (not String.null) <<< String.split (Pattern " ")

-- | Parse a valid slug (as a string) into a `Slug`. This will fail if the
-- | string is not a valid slug and does not provide the same behavior as
-- | `generate`.
-- |
-- | ```purescript
-- | > Slug.parse "my-article-title"
-- | > Just (Slug "my-article-title")
-- |
-- | > Slug.parse "My article"
-- | > Nothing
-- | ```
parse :: String -> Maybe Slug
parse str = generate str >>= check
  where
  check slug@(Slug s)
    | s == str = Just slug
    | otherwise = Nothing

-- | Unwrap a `Slug` into the string contained within, without performing
-- | any transformations.
-- |
-- | ```purescript
-- | > Slug.toString (mySlug :: Slug)
-- | > "my-slug-i-generated"
-- | ```
toString :: Slug -> String
toString (Slug s) = s

-- | Ensure a `Slug` is no longer than a given number of characters. If the last
-- | character is a dash, it will also be removed. Providing a non-positive
-- | number as the length will return `Nothing`.
-- |
-- | ```purescript
-- | > Slug.create "My article title is long!" >>= Slug.truncate 3
-- | > Just (Slug "My")
-- | ```
truncate :: Int -> Slug -> Maybe Slug
truncate n (Slug s)
  | n < 1 = Nothing
  | n >= String.length s = Just (Slug s)
  | otherwise = generate $ String.take n s