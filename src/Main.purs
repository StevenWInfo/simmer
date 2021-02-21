module Main where

import Prelude
import Effect (Effect)

import Repl (runRepl)

main :: Effect Unit
main = runRepl
