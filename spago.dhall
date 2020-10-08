{ name = "halogen-realworld"
, packages = ./packages.dhall
, dependencies =
  [ "aff"
  , "aff-bus"
  , "affjax"
  , "argonaut-core"
  , "codec-argonaut"
  , "console"
  , "debug"
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
  , "variant"
  ]
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
