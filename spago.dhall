{ sources = [ "src/**/*.purs", "test/**/*.purs" ]
, name = "purescript-halogen-realworld"
, packages = ./packages.dhall
, dependencies =
  [ "aff"
  , "aff-bus"
  , "affjax"
  , "argonaut-codecs"
  , "argonaut-core"
  , "codec-argonaut"
  , "console"
  , "effect"
  , "formatters"
  , "halogen"
  , "halogen-formless"
  , "nonempty"
  , "now"
  , "precise-datetime"
  , "prelude"
  , "remotedata"
  , "routing"
  , "routing-duplex"
  , "slug"
  , "struct"
  , "tolerant-argonaut"
  , "typelevel-prelude"
  , "variant"
  ]
}
