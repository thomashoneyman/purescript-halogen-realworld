let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.5-20211116/packages.dhall sha256:7ba810597a275e43c83411d2ab0d4b3c54d0b551436f4b1632e9ff3eb62e327a

in  upstream
  with variant.version = "map-variant"
  with variant.repo = "https://github.com/MonoidMusician/purescript-variant"

  with convertable-options = {
    repo = "https://github.com/natefaubion/purescript-convertable-options",
    version = "v1.0.0",
    dependencies = [ "console", "effect", "maybe", "record" ],
  }

  with halogen-formless = {
    repo = "https://github.com/thomashoneyman/purescript-halogen-formless",
    version = "main",
    dependencies =
      [ "convertable-options"
      , "effect"
      , "either"
      , "foldable-traversable"
      , "foreign-object"
      , "halogen"
      , "heterogeneous"
      , "maybe"
      , "prelude"
      , "record"
      , "safe-coerce"
      , "type-equality"
      , "unsafe-coerce"
      , "unsafe-reference"
      , "variant"
      , "web-events"
      , "web-uievents"
      ]
  }
