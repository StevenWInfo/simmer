module Repl where

import Prelude

import Effect (Effect)

import Node.ReadLine (createConsoleInterface, noCompletion, question)

runRepl :: Effect Unit
runRepl = do
    interface <- createConsoleInterface noCompletion
    interface # question "The REPL isn't complete yet\n" (\answer -> runRepl)
