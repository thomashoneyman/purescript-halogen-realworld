-- | The favorite buttons in Conduit don't have enough encapsulated state or behaviors to be a full
-- | component, but do need to trigger certain actions in a parent component. To avoid writing the
-- | same query handler over and over again, we'll export both the pure HTML function and a default
-- | handle from this module.
-- |
-- | For a more in-depth example of this pattern, see the `Conduit.Component.Part.FollowButton`
-- | module.
module Conduit.Component.Part.FavoriteButton
  ( ButtonSize(..)
  , favoriteButton
  , favorite
  , unfavorite
  ) where

import Prelude

import Conduit.Capability.Resource.Article (class ManageArticle, favoriteArticle, unfavoriteArticle)
import Conduit.Component.HTML.Utils (css)
import Conduit.Data.Article (ArticleWithMetadata)
import Data.Foldable (for_)
import Data.Lens (Traversal', preview, set)
import Data.Maybe (Maybe)
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
  :: forall props act
   . ButtonSize
  -> act
  -> act
  -> ArticleWithMetadata
  -> HH.HTML props act
favoriteButton buttonSize favoriteAct unfavoriteAct article =
  HH.button
    [ css $ "btn btn-sm " <> if article.favorited then "btn-primary" else "btn-outline-primary"
    , HE.onClick \_ -> if article.favorited then unfavoriteAct else favoriteAct
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
            _ -> " (" <> show article.favoritesCount <> ")"
        ]
    ]

-- Eval

favorite
  :: forall st act slots msg m
   . ManageArticle m
  => Traversal' st ArticleWithMetadata
  -> H.HalogenM st act slots msg m Unit
favorite _article = act (not <<< _.favorited) favoriteArticle _article

unfavorite
  :: forall st act slots msg m
   . ManageArticle m
  => Traversal' st ArticleWithMetadata
  -> H.HalogenM st act slots msg m Unit
unfavorite _article = act _.favorited unfavoriteArticle _article

-- This will be kept internal.

act
  :: forall st act slots msg m
   . ManageArticle m
  => (ArticleWithMetadata -> Boolean)
  -> (Slug -> m (Maybe ArticleWithMetadata))
  -> Traversal' st ArticleWithMetadata
  -> H.HalogenM st act slots msg m Unit
act cond f _article = do
  st <- H.get
  for_ (preview _article st) \article -> do
    when (cond article) do
      mbArticle <- H.lift $ f article.slug
      for_ mbArticle $ H.modify_ <<< set _article
