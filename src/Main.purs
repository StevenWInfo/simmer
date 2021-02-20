module Main where

import Prelude

import Effect (Effect)
import Node.ReadLine (createConsoleInterface, noCompletion, question)

main :: Effect Unit
main = do
    interface <- createConsoleInterface noCompletion
    interface # question "The REPL isn't complete yet\n" (\answer -> main)
