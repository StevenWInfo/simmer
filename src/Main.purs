module Main where

import Prelude
import Effect (Effect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Data.Either (Either)

import Repl (runRepl)
import Builtin (builtinLibrary)
import Interpret (Value, eval')

main :: Effect Unit
main = runRepl

runFile :: String -> Effect (Either String Value)
runFile filename = do
    text <- readTextFile UTF8 filename
    eval' [ builtinLibrary ] text
