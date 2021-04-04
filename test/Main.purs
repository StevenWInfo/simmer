module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Aff (launchAff_)

import Test.Spec.Runner (runSpec)
import Test.Spec.Reporter.Console (consoleReporter)

import Test.Parse (parseSuite)
import Test.Interpret (interpretSuite)
import Test.Builtin (builtinSuite)
import Test.Std.Random as Random
import Test.CLI as CLI

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
    parseSuite
    interpretSuite
    CLI.suite
    builtinSuite
    Random.suite
