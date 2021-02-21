module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Exception (Error)
import Control.Monad.Error.Class (class MonadThrow)
import Data.Either (Either(..))
import Text.Parsing.StringParser (runParser)
import Test.Spec (it, pending, describe, SpecT)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Runner (runSpec)
import Test.Spec.Reporter.Console (consoleReporter)

import Parse (digit, integer)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
    --it "does nothing" $ pure unit
    parseSuite

parseSuite :: forall g m. Monad m => MonadThrow Error g => SpecT g Unit m Unit
parseSuite = describe "parseSuite" do
    pending "parse smoke test"
    it "Test digit parser" do
       (runParser digit "3") `shouldEqual` (Right 3)
    it "Test number parser" do
       (runParser integer "14") `shouldEqual` (Right 14)
    it "Test three digit number parser" do
       (runParser integer "789") `shouldEqual` (Right 789)
    it "Test negative number parser" do
       (runParser integer "-37") `shouldEqual` (Right (-37))
