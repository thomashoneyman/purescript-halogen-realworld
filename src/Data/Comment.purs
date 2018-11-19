module Data.Comment where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Author (Author)
import Data.DateTime (DateTime)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)

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

-- Next, we'll define our larger comment data type

type Comment =
  { id :: CommentId 
  , createdAt :: DateTime
  , body :: String
  , author :: Author
  }