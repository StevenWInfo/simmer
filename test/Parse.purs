module Test.Parse where

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

import Ast (Expression(..))
import Parse (numberExpr, stringExpr)

-- TODO put in some tests where parsers should fail.
parseSuite :: forall g m. Monad m => MonadThrow Error g => SpecT g Unit m Unit
parseSuite = describe "parseSuite" do
    pending "parse smoke test"
    it "Test digit parser" do
       (runParser numberExpr "3") `shouldEqual` (Right <<< Number $ 3.0)
    it "Test number parser" do
       (runParser numberExpr "14") `shouldEqual` (Right <<< Number $ 14.0)
    it "Test three digit number parser" do
       (runParser numberExpr "789") `shouldEqual` (Right <<< Number $ 789.0)
    it "Test negative number parser" do
       (runParser numberExpr "-37") `shouldEqual` (Right <<< Number $ (-37.0))
    it "Test float" do
       (runParser numberExpr "37.5") `shouldEqual` (Right <<< Number $ (37.5))
    it "Test String" do
       (runParser stringExpr "\"Lorem ipsum.\"") `shouldEqual` (Right <<< String $ "Lorem ipsum.")
