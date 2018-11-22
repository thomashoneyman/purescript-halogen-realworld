module Api.Endpoint where

-- We'll use a smart constructor once again to provide a reasonable guarantee that
-- the string being passed is a valid endpoint.

import Prelude hiding ((/))

import Data.Comment (CommentId)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Maybe (Maybe(..))
import Data.Route (slug, uname)
import Data.Username (Username)
import Routing.Duplex (RouteDuplex', int, optional, prefix, root, segment, string)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/), (?))
import Slug (Slug)


-- Standard pagination

type Pagination =
  { limit :: Maybe Int 
  , offset :: Maybe Int 
  }

-- Possible parameters that can be passed to the Article endpoint

type ArticleParams =
  { tag :: Maybe String 
  , author :: Maybe Username 
  , favorited :: Maybe Username
  , offset :: Maybe Int
  , limit :: Maybe Int
  }

noArticleParams :: ArticleParams
noArticleParams =
  { tag: Nothing 
  , author: Nothing
  , favorited: Nothing 
  , offset: Nothing
  , limit: Nothing
  }

-- For larger applications, we might want to devise a more sophisticated strategy for
-- managing endpoints. For example, we might derive our client types and functions from
-- a Swagger spec. For the time being, though, we'll use the same approach we did 
-- for our output routes.

data Endpoint
  = Login
  | User
  | Users
  | Follow Username
  | Article Slug
  | Comment Slug CommentId
  | Comments Slug
  | Favorite Slug
  | Articles ArticleParams
  | Profiles Username
  | Feed Pagination
  | Tags

derive instance genericEndpoint :: Generic Endpoint _

instance showEndpoint :: Show Endpoint where
  show = genericShow

-- Our codec will cause a compile-time error if we fail to handle any of our 
-- route cases.

endpointCodec :: RouteDuplex' Endpoint
endpointCodec = root $ prefix "api" $ sum
  { "Login": "users" / "login" / noArgs 
  , "User": "user" / noArgs
  , "Users": "users" / noArgs
  , "Follow": "profiles" / uname segment/ "follow"
  , "Article": "articles" / slug segment
  , "Comments": "articles" / slug segment / "comments"
  , "Comment": "articles" / slug segment / "comments" / commentId
  , "Favorite": "articles" / slug segment / "favorite"
  , "Articles": "articles" ?
      { tag: optional <<< string
      , author: optional <<< uname 
      , favorited: optional <<< uname 
      , offset: optional <<< int
      , limit: optional <<< int
      }
  , "Profiles": "profiles" / uname segment
  , "Feed": "articles" / "feed" ?
      { offset: optional <<< int
      , limit: optional <<< int
      }
  , "Tags": "tags" / noArgs
  }

-- We'll define our own commentId codec

commentId :: RouteDuplex' CommentId
commentId = _Newtype (int segment)