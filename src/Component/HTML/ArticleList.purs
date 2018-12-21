module Conduit.Component.HTML.ArticleList where

import Prelude

import Conduit.Component.HTML.Utils (css, safeHref)
import Conduit.Component.Part.FavoriteButton (favoriteButton, ButtonSize(..))
import Conduit.Data.Article (ArticleWithMetadata)
import Conduit.Data.Author (profile, username)
import Conduit.Data.Avatar as Avatar
import Conduit.Data.PreciseDateTime as PDT
import Conduit.Data.Route (Route(..))
import Conduit.Data.Username as Username
import Data.Array (mapWithIndex)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..))

articleList 
  :: forall i p
   . (Int -> H.Action p)
  -> (Int -> H.Action p)
  -> RemoteData String (Array ArticleWithMetadata) 
  -> HH.HTML i (p Unit)
articleList favoriteQuery unfavoriteQuery = case _ of
  NotAsked -> text "Articles not yet loaded"
  Loading -> text "Loading..."
  Failure err -> text ("Error loading articles: " <> err)
  Success [] -> text "No articles are here...yet!"
  Success articles -> HH.div_ (articlePreview favoriteQuery unfavoriteQuery `mapWithIndex` articles)
  where
  text str = 
    HH.div
      [ css "article-preview" ]
      [ HH.text str ]

-- Provided with a query representing navigation, build an article preview
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
      [ safeHref $ Profile uname ]
      [ HH.img
        [ HP.src $ Avatar.toStringWithDefault avatar
        , HP.alt $ Username.toString uname
        ]
      ]
    , HH.div
      [ css "info" ]
      [ HH.a
        [ css "author", safeHref $ Profile uname ]
        [ HH.text $ Username.toString uname ]
      , HH.span
        [ css "date" ]
        [ HH.text $ PDT.toDisplay article.createdAt ]
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
    uname = username article.author
    avatar = (profile article.author).image

renderTag :: forall i p. String -> HH.HTML i p
renderTag tag =
  HH.li
  [ css "tag-default tag-pill tag-outline" ]
  [ HH.text tag ]
