module Conduit.Component.HTML.ArticleList where

import Prelude

import Conduit.Component.HTML.Utils (css)
import Conduit.Data.Article (Article)
import Conduit.Data.Author (profile, username)
import Conduit.Data.Avatar as Avatar
import Conduit.Data.PreciseDateTime as PDT
import Conduit.Data.Route (Route(..), routeCodec)
import Conduit.Data.Username as Username
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..))
import Routing.Duplex (print)
import Web.Event.Internal.Types (Event)
import Web.UIEvent.MouseEvent (toEvent)

articleList 
  :: forall i p
   . (Route -> Event -> H.Action p) 
  -> RemoteData String (Array Article) 
  -> HH.HTML i (p Unit)
articleList navigate = case _ of
  NotAsked -> HH.text "Not loaded"
  Loading -> HH.text "Loading"
  Failure err -> HH.text ("Error loading articles: " <> err)
  Success articles -> HH.div_ (articlePreview navigate <$> articles)

-- Provided with a query representing navigation, build an article preview
articlePreview :: forall i p. (Route -> Event -> H.Action p) -> Article -> HH.HTML i (p Unit)
articlePreview navigate article =
  HH.div
  [ css "article-preview" ]
  [ HH.div
    [ css "article-meta" ]
    [ HH.a
      [ HE.onClick $ HE.input $ navigate (Profile uname) <<< toEvent 
      , HP.href $ "#" <> print routeCodec (Profile uname) 
      ]
      [ HH.img
        [ HP.src $ Avatar.toStringWithDefault avatar
        , HP.alt $ Username.toString uname
        ]
      ]
    , HH.div
      [ css "info" ]
      [ HH.a
        [ css "author" 
        , HE.onClick $ HE.input $ navigate (Profile uname) <<< toEvent
        , HP.href $ "#" <> print routeCodec (Profile uname)
        ]
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
    , HE.onClick $ HE.input $ navigate (ViewArticle article.slug) <<< toEvent
    , HP.href $ "#" <> print routeCodec (ViewArticle article.slug)
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
