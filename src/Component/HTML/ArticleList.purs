module Conduit.Component.HTML.ArticleList where

import Prelude

import Conduit.Component.HTML.Utils (css, safeHref)
import Conduit.Data.Article (Article)
import Conduit.Data.Author (profile, username)
import Conduit.Data.Avatar as Avatar
import Conduit.Data.PreciseDateTime as PDT
import Conduit.Data.Route (Route(..))
import Conduit.Data.Username as Username
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..))

articleList :: forall i p. RemoteData String (Array Article) -> HH.HTML i p
articleList = case _ of
  NotAsked -> HH.text "Not loaded"
  Loading -> HH.text "Loading"
  Failure err -> HH.text ("Error loading articles: " <> err)
  Success articles -> HH.div_ (articlePreview <$> articles)

-- Provided with a query representing navigation, build an article preview
articlePreview :: forall i p. Article -> HH.HTML i p
articlePreview article =
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
      [ HH.button
        [ css "btn btn-sm btn-outline-primary" ]
        [ HH.i 
          [ css "ion-heart" ] 
          []
        , HH.text $ "\160" <> show article.favoritesCount 
        ]
      ]
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
