module Test.CLI where

import Prelude
import Test.Spec (it, describe, Spec)
import Effect.Class (liftEffect)
import Data.Either (Either(..))
import Test.Spec.Assertions (shouldEqual)

import Interpret as I
import Builtin (builtinLibrary)
import Simmer.CLI (runFile)

suite :: Spec Unit
suite = describe "Tests for Main functionality" do
    simpleTests

stdLib :: Array I.Library
stdLib = ([ builtinLibrary ] )

simpleTests :: Spec Unit
simpleTests = describe "Simple Main functionality tests" do
    it "Test runFile smoke" do
       result <- liftEffect $ runFile stdLib "test/scripts/smoke.smr"
       result `shouldEqual` Right (I.NumberVal 7.0)
