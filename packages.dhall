let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.6-20200123/packages.dhall sha256:687bb9a2d38f2026a89772c47390d02939340b01e31aaa22de9247eadd64af05

let overrides =
      { halogen = upstream.halogen // { version = "v5.0.0-rc.7" }
      , halogen-vdom = upstream.halogen-vdom // { version = "v6.1.0" }
      }

let additions =
  { subcategory =
      { dependencies =
          [ "prelude"
          , "profunctor"
          , "record"
          ]
      , repo =
          "https://github.com/matthew-hilty/purescript-subcategory.git"
      , version =
          "v0.2.0"
      }
  }

in  upstream // overrides // additions
