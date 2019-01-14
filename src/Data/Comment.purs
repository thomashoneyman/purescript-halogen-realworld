-- | Comments are only used when viewing an article. Still, they're an example of an *entity* with
-- | a persistent identity. They're a resource users can manage directly: users can create, delete,
-- | and perhaps eventually modify them.
module Conduit.Data.Comment where

import Prelude

import Conduit.Data.PreciseDateTime (PreciseDateTime)
import Conduit.Data.Profile (Author, decodeAuthor)
import Conduit.Data.Username (Username)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Argonaut.Encode (class EncodeJson)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Traversable (traverse)

-- | This type exists purely as an identifier to distinguish it from a normal `String`, so we'll
-- | create a simple newtype which can be freely wrapped or unwrapped.
newtype CommentId = CommentId Int

derive instance newtypeCommentId :: Newtype CommentId _
derive instance genericCommentId :: Generic CommentId _
derive instance eqCommentId :: Eq CommentId
derive instance ordCommentId :: Ord CommentId

derive newtype instance encodeJsonCommentId :: EncodeJson CommentId
derive newtype instance decodeJsonCommentId :: DecodeJson CommentId

instance showCommentId :: Show CommentId where
  show = genericShow

-- | Next, we'll define our larger comment type. A comment consists of a persistent unique
-- | identifier, a timestamp, a user-created body, and information about the user who created   
-- | the comment.
type Comment =
  { id :: CommentId 
  , createdAt :: PreciseDateTime
  , body :: String
  , author :: Author
  }

-- | Most records can be generically encoded or decoded without us having to write anything 
-- | manually. In this case, though, there is a field that can't be handled generically:
-- | an author, which requires a username to be decoded.
-- |
-- | So we'll go ahead and write these small instances manually. First, we'll write a decoder for
-- | an array of comments.
decodeComments :: Maybe Username -> Json -> Either String (Array Comment)
decodeComments u = traverse (decodeComment u) <=< (_ .:  "comments") <=< decodeJson

-- | This decoder operates on a single comment.
decodeComment :: Maybe Username -> Json -> Either String Comment
decodeComment u json = do
  obj <- decodeJson json
  author <- decodeAuthor u =<< obj .: "author"
  body <- obj .: "body"
  id <- obj .: "id"
  createdAt <- obj .: "createdAt"
  pure { id, createdAt, body, author }