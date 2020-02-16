-- | Articles are an example of an entity (a persistent data type with a unique identity). They're
-- | a critical, widely-used type in Conduit.
-- |
-- | We have two versions of the `Article` type: one with just some base fields and another with
-- | the same fields, but also a bunch of metadata that is only sometimes included in the data
-- | we get from the server.
-- |
-- | It's tedious and error-prone to define multiple variations of a type which can have fields
-- | that are only sometimes present. To avoid this, we'll use PureScript's lovely extensible
-- | rows & records. I've given a *much* fuller treatment of the topic in the `Conduit.Api.Request`
-- | module. If you stil feel unfamiliar with this pattern, I recommend giving that module a read
-- | before coming back to this one.
module Conduit.Data.Article where

import Prelude

import Conduit.Data.PaginatedArray (PaginatedArray)
import Conduit.Data.PreciseDateTime (PreciseDateTime)
import Conduit.Data.Profile (Author, decodeJsonWithAuthor)
import Conduit.Data.Username (Username)
import Conduit.Data.Utils (decodeAt)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson, (.:))
import Data.Array (filter)
import Data.Either (Either, isRight)
import Data.Maybe (Maybe)
import Data.Traversable (sequence)
import Slug (Slug)
import Type.Row (type (+))

-- | First, we'll describe the core fields that are always present when we have an `Article`. In
-- | general these fields can be edited by users and have few-to-no restrictions on the type. Just
-- | about any string will do, according to the spec.
type ArticleRep row =
  ( title :: String
  , description :: String
  , body :: String
  , tagList :: Array String
  | row
  )

-- | Next, we'll define another row containing the additional metadata which is sometimes included
-- | with an article. These types are not generally editable by users and have crisper types. For
-- | example, we expect a well-formed slug (not any string will do!), a precise datetime value,
-- | and a custom `Profile` type. The `Profile` type is interesting because it disallows some tricky
-- | invalid states in the type system. It's defined in `Conduit.Data.Profile`.
type ArticleMetadataRep row =
  ( slug :: Slug
  , createdAt :: PreciseDateTime
  , favorited :: Boolean
  , favoritesCount :: Int
  , author :: Author
  | row
  )

-- | The prior types were both rows, which are type-level lists. We can't actually construct a
-- | value-level row! Instead, we'll create records, which are similar to objects or dictionaries
-- | in other languages.
-- |
-- | Our article type will be a record containing only the core row. Our extended article type,
-- | `ArticleWithMetadat`, will include all the fields from the core row, plus all the fields from
-- | the metadat row.
type Article = { | ArticleRep () }
type ArticleWithMetadata = { | ArticleRep + ArticleMetadataRep () }

-- | We're going to send and receive articles in the JSON format all the time. Most of the time,
-- | libraries like Argonaut or Simple JSON can just encode and decode records without needing
-- | anything manually written out. You get the `DecodeJson` and `EncodeJson` instances for free!
-- |
-- | For example, we don't need to write a decoder or encoder for the `Article` type. It's already
-- | done for us.
-- |
-- | Unfortunately, our custom `Author` type cannot be generically encoded and decoded like this
-- | because it requires knowing the username of the current user (if there is one) to determine
-- | you follow this particular author.
-- |
-- | So we'll write the decoder for the `Article` type manually.
-- |
-- | We have two decoding options we can consider:
-- |
-- | 1. using `decodeJsonWith` from the 'tolerant-argonaut' package and manually decoding just
-- | the `articles` field
-- |
-- | ```purescript
-- | decodeArticles
-- |   :: Maybe Username
-- |   -> Json
-- |   -> Either String { articles :: Array ArticleWithMetadata, articlesCount :: Int }
-- | decodeArticles u =
-- |    decodeJsonWith { articles: decodeArticles' <=< decodeJson }
-- |  where
-- |  decodeArticles' = sequence <<< filter isRight <<< map (decodeJsonWithAuthor u)
-- | ```
-- |
-- | 2. manually decoding every field (shown below)
-- |
-- | The first method has the advantage of easily scaling to accommodate larger record types.
-- | However, as a tradeoff, adapting the field names of incoming JSON data becomes more difficult.
-- | For example, changing the field names to convert JSON to instances of `PaginatedArray
-- | ArticleWithMetadata` might be done as follows:
-- |
-- | ```purescript
-- | decodeArticles :: Maybe Username -> Json -> Either String (PaginatedArray ArticleWithMetadata)
-- | decodeArticles u = map renameFields <<< decode
-- |   where
-- |   renameFields =
-- |     rrenameMany
-- |       { articles: SProxy :: SProxy "body"
-- |       , articlesCount: SProxy :: SProxy "total"
-- |       }
-- |   decode = decodeJsonWith { articles: decodeArticles' <=< decodeJson }
-- |   decodeArticles' = sequence <<< filter isRight <<< map (decodeJsonWithAuthor u)
-- | ```
-- |
-- | Because `PaginatedArray ArticleWithMetadata` is a relatively simple type, we opt for the
-- | second method, and we manually decode every field.
decodeArticles :: Maybe Username -> Json -> Either String (PaginatedArray ArticleWithMetadata)
decodeArticles u json = do
  obj <- decodeJson json
  arr <- obj .: "articles"
  total <- obj .: "articlesCount"
  -- For now, we'll drop out malformed articles. The server shouldn't send us bad data, and we
  -- could have a more sophisticated response, but this will do for our MVP.
  filteredArr <- sequence $ filter isRight $ map (decodeJsonWithAuthor u) arr
  pure { body: filteredArr, total }

-- | This helper function decodes a single `ArticleWithMetadata` at the key "article".
decodeArticle :: Maybe Username -> Json -> Either String ArticleWithMetadata
decodeArticle u = decodeJsonWithAuthor u <=< decodeAt "article"
