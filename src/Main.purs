module Main where

import Prelude
import Effect (Effect)

-- import Repl (runRepl)
import Builtin (builtinLibrary)
import Std (libraries)
import Simmer.CLI (cli)

main :: Effect Unit
main = cli ([ builtinLibrary ] <> libraries)
