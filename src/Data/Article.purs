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

import Conduit.Data.PaginatedArray (PaginatedArray)
import Conduit.Data.PreciseDateTime as PDT
import Conduit.Data.Profile (Author)
import Conduit.Data.Profile as Profile
import Conduit.Data.Username (Username)
import Data.Codec ((>~>))
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Migration as CAM
import Data.Codec.Argonaut.Record as CAR
import Data.Maybe (Maybe)
import Data.PreciseDateTime (PreciseDateTime)
import Slug (Slug)
import Slug as Slug
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

articleCodec :: JsonCodec Article
articleCodec =
  CAR.object "Article"
    { title: CA.string
    , body: CA.string
    , description: CA.string
    , tagList: CA.array CA.string
    }

type ArticleWithMetadata = { | ArticleRep + ArticleMetadataRep () }

articleWithMetadataCodec :: Maybe Username -> JsonCodec ArticleWithMetadata
articleWithMetadataCodec mbUsername =
  CAR.object "ArticleWithMetadata"
    { title: CA.string
    , body: CA.string
    , description: CA.string
    , tagList: CA.array CA.string
    , slug: CA.prismaticCodec "Slug" Slug.parse Slug.toString CA.string
    , createdAt: PDT.codec
    , favorited: CA.boolean
    , favoritesCount: CA.int
    , author: Profile.authorCodec mbUsername
    }

articlesWithMetadataCodec :: Maybe Username -> JsonCodec (PaginatedArray ArticleWithMetadata)
articlesWithMetadataCodec mbUsername =
  CAM.renameField "articles" "body"
    >~> CAM.renameField "articlesCount" "total"
    >~> codec
  where
  codec =
    CAR.object "PaginatedArray ArticleWithMetadata"
      { body: CA.array (articleWithMetadataCodec mbUsername)
      , total: CA.int
      }
