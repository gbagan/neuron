let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.8-20230511/packages.dhall
        sha256:47cfb0e7e14ee01dee00cb64bd0ca4a47ac44a64f3edf1e774251fb44cd73beb

let additions = 
      { relude =
        { dependencies =
          [ "aff"
          , "arrays"
          , "control"
          , "effect"
          , "either"
          , "foldable-traversable"
          , "generate-values"
          , "integers"
          , "lazy"
          , "lists"
          , "maybe"
          , "numbers"
          , "ordered-collections"
          , "prelude"
          , "profunctor-lenses"
          , "transformers"
          , "tuples"
          , "unfoldable"
          ]
        , repo = "https://github.com/gbagan/purescript-relude.git"
        , version = "master"
      }
    }


in  upstream // additions
