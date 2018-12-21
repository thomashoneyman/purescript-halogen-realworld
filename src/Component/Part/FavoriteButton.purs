module Conduit.Component.Part.FavoriteButton 
  ( ButtonSize(..)
  , favoriteButton
  , favorite
  , unfavorite
  ) where

import Prelude

import Conduit.Capability.LogMessages (class LogMessages, logError)
import Conduit.Capability.ManageResource (class ManageAuthResource, favoriteArticle, unfavoriteArticle)
import Conduit.Component.HTML.Utils (css)
import Conduit.Data.Article (ArticleWithMetadata)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Lens (Traversal', preview, set)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Slug (Slug)

-- A simple way to control button sizes

data ButtonSize
  = Icon
  | Medium

derive instance eqButtonSize :: Eq ButtonSize 

favoriteButton 
  :: forall i p
   . ButtonSize 
  -> H.Action p 
  -> H.Action p 
  -> ArticleWithMetadata 
  -> HH.HTML i (p Unit)
favoriteButton buttonSize favoriteQuery unfavoriteQuery article =
  HH.button
    [ css $ "btn btn-sm " <> if buttonSize == Icon then "btn-outline-primary" else "btn-primary"
    , HE.onClick $ HE.input_ $ if article.favorited then unfavoriteQuery else favoriteQuery
    ]
    [ HH.i 
      [ css "ion-heart" ]
      []
    , HH.span_
      [ HH.text $ case article.favorited, buttonSize of 
          true, Medium -> " Unfavorite Article" 
          _, Medium -> " Favorite Article" 
          _, _ -> " "
      ]
    , HH.span
      [ css "counter" ]
      [ HH.text $ case buttonSize of
          Icon -> " " <> show article.favoritesCount
          _ -> " (" <> show article.favoritesCount <> ")" ]
    ]

-- Eval

favorite  
  :: forall s f g p o m
   . ManageAuthResource m
  => LogMessages m
  => Traversal' s ArticleWithMetadata
  -> H.HalogenM s f g p o m Unit
favorite _article = act (not <<< _.favorited) favoriteArticle _article

unfavorite  
  :: forall s f g p o m
   . ManageAuthResource m
  => LogMessages m
  => Traversal' s ArticleWithMetadata
  -> H.HalogenM s f g p o m Unit
unfavorite _article = act _.favorited unfavoriteArticle _article

-- This will be kept internal.

act  
  :: forall s f g p o m
   . ManageAuthResource m
  => LogMessages m
  => (ArticleWithMetadata -> Boolean)
  -> (Slug -> m (Either String ArticleWithMetadata))
  -> Traversal' s ArticleWithMetadata
  -> H.HalogenM s f g p o m Unit
act cond f _article = do
  st <- H.get
  for_ (preview _article st) \article -> do
    when (cond article) do
      new <- H.lift $ f article.slug
      case new of
        Left str -> logError str
        Right newArticle -> 
          H.modify_ (set _article newArticle)