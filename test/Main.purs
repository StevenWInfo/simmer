module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Data.Either (Either(..))

import Test.Spec (it, describe, Spec)
import Test.Spec.Runner (runSpec)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Assertions (shouldEqual)

import Test.Parse (parseSuite)
import Test.Interpret (interpretSuite)
import Test.Builtin (builtinSuite)
import Test.Std.Random as Random

import Builtin (builtinLibrary)
import Std (libraries)
import Main (runFile)
import Interpret as I

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
    parseSuite
    interpretSuite
    mainSuite
    builtinSuite
    Random.suite

mainSuite :: Spec Unit
mainSuite = describe "Tests for Main functionality" do
    simpleTests

stdLib :: Array I.Library
stdLib = ([ builtinLibrary ] <> libraries)

simpleTests :: Spec Unit
simpleTests = describe "Simple Main functionality tests" do
    it "Test runFile smoke" do
       result <- liftEffect $ runFile stdLib "test/scripts/smoke.smr"
       result `shouldEqual` Right (I.NumberVal 7.0)
