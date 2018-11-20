module Data.Comment where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.?))
import Data.Argonaut.Encode (class EncodeJson)
import Data.Author (Author, decodeAuthor)
import Data.DateTime (DateTime)
import Data.Either (Either)
import Data.Formatter.DateTime (unformatDateTime)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import Data.Traversable (traverse)
import Data.Username (Username)

-- We'll newtype `CommentId` without putting any constraints on its value purely as an 
-- identifier to distinguish it from strings. 

newtype CommentId = CommentId Int

derive instance newtypeCommentId :: Newtype CommentId _
derive instance genericCommentId :: Generic CommentId _
derive instance eqCommentId :: Eq CommentId
derive instance ordCommentId :: Ord CommentId

derive newtype instance encodeJsonCommentId :: EncodeJson CommentId
derive newtype instance decodeJsonCommentId :: DecodeJson CommentId

instance showCommentId :: Show CommentId where
  show = genericShow

-- Next, our small comment update type

type CreateComment = { body :: String }

-- Next, we'll define our larger comment data type

type Comment =
  { id :: CommentId 
  , createdAt :: DateTime
  , body :: String
  , author :: Author
  }

-- This manual instance is necessary because there is no instance  for an author
-- or datetime; we'll need additional information for decoding than the data type
-- alone, though generic decoding for records is supported.

decodeComments :: Username -> Json -> Either String (Array Comment)
decodeComments u json = do
  arr <- decodeJson json 
  traverse (decodeComment u) arr

decodeComment :: Username -> Json -> Either String Comment
decodeComment u json = do
  obj <- decodeJson json
  author <- decodeAuthor u =<< obj .? "author"
  body <- obj .? "body"
  id <- obj .? "id"
  createdAt <- unformatDateTime "X" =<< obj .? "createdAt"
  pure { id, createdAt, body, author }