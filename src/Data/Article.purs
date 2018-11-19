module Data.Article where

import Data.Author (Author)
import Data.DateTime (DateTime)
import Slug (Slug)

-- Next, we'll define our larger comment data type

type Article =
  { slug :: Slug 
  , title :: String
  , description :: String
  , body :: String
  , tagList :: Array String
  , createdAt :: DateTime
  , favorited :: Boolean
  , favoritesCount :: Int
  , author :: Author
  }
