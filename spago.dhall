{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "rough"
, dependencies =
  [ "console"
  , "debug"
  , "effect"
  , "node-fs"
  , "node-readline"
  , "numbers"
  , "ordered-collections"
  , "psci-support"
  , "purescript-yarn"
  , "spec"
  , "string-parsers"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
