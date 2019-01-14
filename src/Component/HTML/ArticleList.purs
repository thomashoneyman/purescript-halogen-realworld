-- | This module exports a pure HTML function to render lists of articles in various formats,
-- | included paginated lists.
module Conduit.Component.HTML.ArticleList where

import Prelude

import Conduit.Component.HTML.Utils (css, safeHref, whenElem)
import Conduit.Component.Part.FavoriteButton (favoriteButton, ButtonSize(..))
import Conduit.Data.Article (ArticleWithMetadata)
import Conduit.Data.Avatar as Avatar
import Conduit.Data.PaginatedArray (PaginatedArray)
import Conduit.Data.PreciseDateTime as PDT
import Conduit.Data.Route (Route(..))
import Conduit.Data.Username as Username
import Data.Array (mapWithIndex)
import Data.Enum (enumFromTo)
import Data.Foldable (length)
import Data.Monoid (guard)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..))
import Web.UIEvent.MouseEvent (MouseEvent)

articleList 
  :: forall i p
   . (Int -> H.Action p)
  -> (Int -> H.Action p)
  -> RemoteData String (PaginatedArray ArticleWithMetadata) 
  -> HH.HTML i (p Unit)
articleList favoriteQuery unfavoriteQuery = case _ of
  NotAsked -> 
    text "Articles not yet loaded"
  Loading -> 
    text "Loading..."
  Failure err -> 
    text ("Error loading articles: " <> err)
  Success { body } | length body == 0 -> 
    text "No articles are here...yet!"
  Success articles -> 
    HH.div_ 
      (articlePreview favoriteQuery unfavoriteQuery `mapWithIndex` articles.body)
  where
  text str = 
    HH.div
      [ css "article-preview" ]
      [ HH.text str ]

articlePreview 
  :: forall i p
   . (Int -> H.Action p)
  -> (Int -> H.Action p)
  -> Int
  -> ArticleWithMetadata 
  -> HH.HTML i (p Unit)
articlePreview favoriteQuery unfavoriteQuery ix article =
  HH.div
  [ css "article-preview" ]
  [ HH.div
    [ css "article-meta" ]
    [ HH.a
      [ safeHref $ Profile username ]
      [ HH.img
        [ HP.src $ Avatar.toStringWithDefault avatar
        , HP.alt $ Username.toString username
        ]
      ]
    , HH.div
      [ css "info" ]
      [ HH.a
        [ css "author", safeHref $ Profile username ]
        [ HH.text $ Username.toString username ]
      , HH.span
        [ css "date" ]
        [ HH.text $ PDT.toDisplayWeekName article.createdAt ]
      ]
    , HH.div
      [ css "pull-xs-right" ]
      [ favoriteButton Icon (favoriteQuery ix) (unfavoriteQuery ix) article ]
    ]
  , HH.a
    [ css "preview-link" 
    , safeHref $ ViewArticle article.slug
    ]
    [ HH.h1_ 
        [ HH.text article.title ]
    , HH.p_ 
        [ HH.text article.description ]
    , HH.span_ 
        [ HH.text "Read more..." ]
    , HH.ul
        [ css "tag-list" ]
        (article.tagList <#> renderTag)
    ]
  ]
  where
    username = article.author.username
    avatar = article.author.image

renderTag :: forall i p. String -> HH.HTML i p
renderTag tag =
  HH.li
  [ css "tag-default tag-pill tag-outline" ]
  [ HH.text tag ]

-- Pagination

renderPagination :: forall i p. (Int -> MouseEvent -> H.Action p) -> Int -> PaginatedArray ArticleWithMetadata -> HH.HTML i (p Unit)
renderPagination query currentIndex { body, total } =
  whenElem (total > 20) \_ ->
    HH.ul  
      [ css "pagination" ]
      (renderPageLink query currentIndex <$> enumFromTo 1 (total / 20))

renderPageLink :: forall i p. (Int -> MouseEvent -> H.Action p) -> Int -> Int -> HH.HTML i (p Unit)
renderPageLink query activeIndex index =
  HH.li
    [ css $ "page-item" <> guard (activeIndex == index) " active" ]
    [ HH.a 
      [ css "page-link"
      , HP.href "" -- needed for realworld css; remember to prevent default! 
      , HE.onClick $ HE.input $ query index
      ]
      [ HH.text $ show index ]
    ]