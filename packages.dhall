let mkPackage =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.2-20190725/src/mkPackage.dhall sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let upstream =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.2-20190725/src/packages.dhall sha256:60cc03d2c3a99a0e5eeebb16a22aac219fa76fe6a1686e8c2bd7a11872527ea3


let overrides =
      { halogen =
          upstream.halogen // { version = "v5.0.0-rc.5" }
      , halogen-vdom =
          upstream.halogen-vdom // { version = "v6.1.0" }
      }

let additions =
      { halogen-formless =
          mkPackage
          [ "halogen"
          , "variant"
          , "heterogeneous"
          , "generics-rep"
          , "profunctor-lenses"
          ]
          "https://github.com/thomashoneyman/purescript-halogen-formless.git"
          "v1.0.0-rc.1"
      , slug =
          mkPackage
          [ "prelude"
          , "maybe"
          , "strings"
          , "unicode"
          , "generics-rep"
          , "argonaut-codecs"
          ]
          "https://github.com/thomashoneyman/purescript-slug.git"
          "v1.0.0"
      , precise-datetime =
          mkPackage
          [ "arrays"
          , "console"
          , "datetime"
          , "either"
          , "enums"
          , "foldable-traversable"
          , "formatters"
          , "integers"
          , "js-date"
          , "lists"
          , "maybe"
          , "newtype"
          , "prelude"
          , "strings"
          , "tuples"
          , "unicode"
          , "numbers"
          , "decimals"
          ]
          "https://github.com/awakesecurity/purescript-precise-datetime.git"
          "v5.1.1"
      , higher-order =
          mkPackage
            [ "catenable-lists"
            , "const"
            , "effect"
            , "errors"
            , "generics-rep"
            , "lists"
            , "ordered-collections"
            , "orders"
            , "profunctor"
            ]
            "https://github.com/matthew-hilty/purescript-higher-order.git"
            "v0.2.0"
      , proxying =
          mkPackage
            [ "console"
            , "effect"
            , "generics-rep"
            , "prelude"
            , "test-unit"
            , "typelevel-prelude"
            ]
            "https://github.com/matthew-hilty/purescript-proxying.git"
            "v1.1.0"
      , struct =
          mkPackage
            [ "argonaut"
            , "argonaut-codecs"
            , "console"
            , "effect"
            , "proxying"
            , "record"
            , "record-extra"
            , "subcategory"
            , "test-unit"
            , "variant"
            ]
            "https://github.com/matthew-hilty/purescript-struct.git"
            "v1.1.0"
      , subcategory =
          mkPackage
            [ "prelude"
            , "profunctor"
            , "record"
            ]
            "https://github.com/matthew-hilty/purescript-subcategory.git"
            "v0.2.0"
      , tolerant-argonaut =
          mkPackage
            [ "argonaut-codecs"
            , "argonaut-core"
            , "arrays"
            , "console"
            , "effect"
            , "higher-order"
            , "lists"
            , "psci-support"
            , "record"
            , "struct"
            , "typelevel-prelude"
            ]
            "https://github.com/matthew-hilty/purescript-tolerant-argonaut.git"
            "v1.1.0"
      }

in  upstream // overrides // additions
