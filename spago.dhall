{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "simmer"
, dependencies =
  [ "console"
  , "debug"
  , "effect"
  , "node-fs"
  , "node-readline"
  , "numbers"
  , "optparse"
  , "ordered-collections"
  , "psci-support"
  --, "purescript-yarn"
  , "random"
  , "spec"
  , "string-parsers"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
