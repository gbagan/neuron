{ name = "purescript"
, dependencies =
  [ "arrays"
  , "effect"
  , "foldable-traversable"
  , "integers"
  , "lazy"
  , "maybe"
  , "numbers"
  , "pha"
  , "prelude"
  , "profunctor-lenses"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
