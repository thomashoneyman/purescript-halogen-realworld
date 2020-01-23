let mkPackage =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.2-20190725/src/mkPackage.dhall sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.5-20200103/packages.dhall sha256:0a6051982fb4eedb72fbe5ca4282259719b7b9b525a4dda60367f98079132f30


let overrides =
      { halogen = upstream.halogen // { version = "v5.0.0-rc.7" }
      , halogen-vdom = upstream.halogen-vdom // { version = "v6.1.0" }
      }

let additions =
      { subcategory =
          mkPackage
            [ "prelude", "profunctor", "record" ]
            "https://github.com/matthew-hilty/purescript-subcategory.git"
            "v0.2.0"
      }

in  upstream // overrides // additions
