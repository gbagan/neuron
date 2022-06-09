{ name = "purescript"
, dependencies =
  [ "arrays"
  , "effect"
  , "foldable-traversable"
  , "integers"
  , "lazy"
  , "maybe"
  , "pha"
  , "prelude"
  , "profunctor-lenses"
  , "web-events"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
