-- | Comments are only used when viewing an article. Still, they're an example of an *entity* with
-- | a persistent identity. They're a resource users can manage directly: users can create, delete,
-- | and perhaps eventually modify them.
module Conduit.Data.Comment where

import Prelude

import Conduit.Data.PreciseDateTime as PDT
import Conduit.Data.Profile (Author)
import Conduit.Data.Profile as Profile
import Conduit.Data.Username (Username)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.PreciseDateTime (PreciseDateTime)
import Data.Profunctor (wrapIso)

-- | This type exists purely as an identifier to distinguish it from a normal `String`, so we'll
-- | create a simple newtype which can be freely wrapped or unwrapped.
newtype CommentId = CommentId Int

derive instance newtypeCommentId :: Newtype CommentId _
derive instance eqCommentId :: Eq CommentId
derive instance ordCommentId :: Ord CommentId

-- | Next, we'll define our larger comment type. A comment consists of a persistent unique
-- | identifier, a timestamp, a user-created body, and information about the user who created
-- | the comment.
type Comment = { body :: String }

type CommentWithMetadata =
  { id :: CommentId
  , createdAt :: PreciseDateTime
  , body :: String
  , author :: Author
  }

commentCodec :: JsonCodec Comment
commentCodec =
  CAR.object "Comment" { body: CA.string }

commentWithMetadataCodec :: Maybe Username -> JsonCodec CommentWithMetadata
commentWithMetadataCodec mbUsername =
  CAR.object "CommentWithMetadata"
    { id: wrapIso CommentId CA.int
    , createdAt: PDT.codec
    , body: CA.string
    , author: Profile.authorCodec mbUsername
    }
