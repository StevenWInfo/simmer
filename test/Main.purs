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

import Main (runFile)
import Interpret as I

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
    parseSuite
    interpretSuite
    mainSuite

mainSuite :: Spec Unit
mainSuite = describe "Tests for Main functionality" do
    simpleTests

simpleTests :: Spec Unit
simpleTests = describe "Simple Main functionality tests" do
    it "Test runFile smoke" do
       result <- liftEffect $ runFile "test/scripts/smoke.smr"
       result `shouldEqual` Right (I.NumberVal 7.0)
