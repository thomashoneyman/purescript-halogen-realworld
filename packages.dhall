let mkPackage =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.12.5-20190427/src/mkPackage.dhall sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let upstream =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.12.5-20190427/src/packages.dhall sha256:6b17811247e1f825034fa4dacc4b8ec5eddd0e832e0e1579c2ba3b9b2a1c63fe

let overrides =
      { halogen =
          upstream.halogen // { version = "v5.0.0-rc.4" }
      , halogen-vdom =
          upstream.halogen-vdom // { version = "v6.1.0" }
      , remotedata =
          upstream.remotedata // { version = "v4.2.0" }
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
          "halogen-5"
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
      }

in  upstream // overrides // additions
