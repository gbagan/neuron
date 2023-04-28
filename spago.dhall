{ name = "purescript"
, dependencies =
  [ "aff"
  , "arrays"
  , "datetime"
  , "effect"
  , "foldable-traversable"
  , "integers"
  , "maybe"
  , "numbers"
  , "partial"
  , "pha"
  , "prelude"
  , "profunctor-lenses"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
