{ name = "purescript"
, dependencies =
  [ "arrays"
  , "datetime"
  , "foldable-traversable"
  , "integers"
  , "numbers"
  , "partial"
  , "pha"
  , "prelude"
  , "relude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
