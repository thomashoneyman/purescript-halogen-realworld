module Component.HTML.ArticleList where

import Prelude

import Component.Classes as CC
import Data.Article (Article)
import Data.Author (profile, username)
import Data.Formatter.DateTime (FormatterCommand(..), format)
import Data.List (List, fromFoldable)
import Data.Maybe (maybe)
import Data.Profile (avatarToString)
import Data.Username (toString)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap4 as HB
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
  [ HP.class_ CC.articlePreview ]
  [ HH.div
    [ HP.class_ CC.articleMeta ]
    [ HH.a_
      [ HH.img
        [ HP.src $ a.author # profile # _.avatar # maybe "" avatarToString
        , HP.alt $ toString $ username $ a.author
        ]
      ]
    , HH.div
      [ HP.class_ CC.info ]
      [ HH.a [ HP.class_ CC.author ] [ HH.text $ toString $ username a.author ]
      , HH.span [ HP.class_ CC.date ] [ HH.text $ format dateFormatter $ a.createdAt ]
      ]
    , HH.div
      [ HP.class_ $ HH.ClassName "pull-xs-right" ]
      [ HH.button
        [ HP.classes [ HB.btn, HB.btnSm, HB.btnOutlinePrimary ] ]
        [ HH.i [ HP.class_ $ HH.ClassName "ion-heart" ] []
        , HH.text $ "\160" <> show a.favoritesCount 
        ]
      ]
    ]
  , HH.a
    [ HP.class_ CC.previewLink ]
    [ HH.h1_ [ HH.text a.title ]
    , HH.p_ [ HH.text a.description ]
    , HH.span_ [ HH.text "Read more..." ]
    , HH.ul
      [ HP.class_ CC.tagList ]
      $ a.tagList <#> \tag -> HH.li
                              [ HP.classes [ CC.tagDefault, CC.tagPill, CC.tagOutline ] ]
                              [ HH.text tag ]
    ]
  ]

         
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
