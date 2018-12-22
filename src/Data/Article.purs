module Conduit.Data.Article where

import Prelude

import Conduit.Data.Author (Author, decodeAuthor)
import Conduit.Data.PaginatedArray (PaginatedArray)
import Conduit.Data.PreciseDateTime (PreciseDateTime)
import Conduit.Data.Username (Username)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson, (.:))
import Data.Array (filter)
import Data.Either (Either, isRight)
import Data.Maybe (Maybe)
import Data.Traversable (sequence)
import Slug (Slug)
import Type.Row (type (+))

-- Our article will be made up of two segments merged together: some base fields which
-- can be set and modified, and some metadata fields.

-- First, we'll describe the different variations:

type ArticleWithMetadata = { | ArticleRep + ArticleMetadataRep () }

type Article = { | ArticleRep () }

-- Next, we'll define our row containing the core descriptive fields for an article

type ArticleRep r =
  ( title :: String
  , description :: String
  , body :: String
  , tagList :: Array String
  | r
  )

-- Finally, we'll define our row containing non-editable metadata fields

type ArticleMetadataRep r =
  ( slug :: Slug 
  , createdAt :: PreciseDateTime
  , favorited :: Boolean
  , favoritesCount :: Int
  , author :: Author
  | r
  )

-- This manual instance is necessary because there is no instance  for an author
-- or datetime; we'll need additional information for decoding than the data type
-- alone, though generic decoding for records is supported.

decodeArticles :: Maybe Username -> Json -> Either String (PaginatedArray ArticleWithMetadata)
decodeArticles u json = do
  obj <- decodeJson json 
  arr <- obj .: "articles"
  total <- obj .: "articlesCount"
  -- for now, we'll drop out malformed articles. the server shouldn't even
  -- send us bad data, and we could have a more sophisticated response, but
  -- this will do for now.
  filteredArr <- sequence $ filter isRight $ map (decodeArticleWithMetadata' u) arr
  pure { body: filteredArr, total }

decodeArticle :: Maybe Username -> Json -> Either String ArticleWithMetadata
decodeArticle u json = do
  obj <- (_ .: "article") =<< decodeJson json
  decodeArticleWithMetadata' u obj 

decodeArticleWithMetadata' :: Maybe Username -> Json -> Either String ArticleWithMetadata
decodeArticleWithMetadata' u json = do
  obj <- decodeJson json
  slug <- obj .: "slug"
  title <- obj .: "title"
  body <- obj .: "body"
  description <- obj .: "description"
  tagList <- obj .: "tagList"
  favorited <- obj .: "favorited"
  favoritesCount <- obj .: "favoritesCount"
  createdAt <- obj .: "createdAt" 
  author <- decodeAuthor u =<< obj .: "author"
  pure { slug, title, body, description, tagList, createdAt, favorited, favoritesCount, author }
