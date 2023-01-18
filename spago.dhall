{ name = "purescript"
, dependencies =
  [ "arrays"
  , "effect"
  , "foldable-traversable"
  , "integers"
  , "maybe"
  , "numbers"
  , "partial"
  , "pha"
  , "prelude"
  , "profunctor-lenses"
  , "unfoldable"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
