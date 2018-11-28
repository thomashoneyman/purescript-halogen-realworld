module Component.HTML.ArticleList where

import Prelude

import Component.HTML.Utils (css)
import Data.Article (Article)
import Data.Author (profile, username)
import Data.Formatter.DateTime (FormatterCommand(..), format)
import Data.List (List, fromFoldable)
import Data.Maybe (maybe)
import Data.Profile (avatarToString)
import Data.Username (toString)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..))

articleList :: forall p i. RemoteData String (Array Article) -> HH.HTML p i
articleList = case _ of
  NotAsked -> HH.text "Not loaded"
  Loading -> HH.text "Loading"
  Failure err -> HH.text $ "Error loading articles: " <> err
  Success articles -> HH.div_ $ articles <#> article

article :: forall p i. Article -> HH.HTML p i
article a =
  HH.div
  [ css "article-preview" ]
  [ HH.div
    [ css "article-meta" ]
    [ HH.a_
      [ HH.img
        [ HP.src $ a.author # profile # _.avatar # maybe "" avatarToString
        , HP.alt $ toString $ username $ a.author
        ]
      ]
    , HH.div
      [ css "info" ]
      [ HH.a
        [ css "author" ]
        [ HH.text $ toString $ username a.author ]
      , HH.span
        [ css "date" ]
        [ HH.text $ format dateFormatter $ a.createdAt ]
      ]
    , HH.div
      [ css "pull-xs-right" ]
      [ HH.button
        [ css "btn btn-sm btn-outline-primary" ]
        [ HH.i [ css "ion-heart" ] []
        , HH.text $ "\160" <> show a.favoritesCount 
        ]
      ]
    ]
  , HH.a
    [ css "preview-link" ]
    [ HH.h1_ [ HH.text a.title ]
    , HH.p_ [ HH.text a.description ]
    , HH.span_ [ HH.text "Read more..." ]
    , HH.ul
      [ css "tag-list" ]
      $ a.tagList <#> renderTag
    ]
  ]

renderTag :: forall p i. String -> HH.HTML p i
renderTag tag =
  HH.li
  [ css "tag-default tag-pill tag-outline" ]
  [ HH.text tag ]

         
dateFormatter :: List FormatterCommand
dateFormatter = fromFoldable
  [ DayOfWeekNameShort
  , Placeholder " "
  , MonthShort
  , Placeholder " "
  , DayOfMonth
  , Placeholder " "
  , YearFull
  ]
