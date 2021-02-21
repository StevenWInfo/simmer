module Test.Parse where

import Prelude
import Effect.Exception (Error)
import Control.Monad.Error.Class (class MonadThrow)
import Data.Either (Either(..))
import Text.Parsing.StringParser (runParser, ParseError(..))
import Test.Spec (it, pending', describe, SpecT)
import Test.Spec.Assertions (shouldEqual)

import Ast (Expression(..))
import Parse (numberExpr, stringExpr, removeComments, expression, parse)

-- TODO put in some tests where parsers should fail.
parseSuite :: forall g m. Monad m => MonadThrow Error g => SpecT g Unit m Unit
parseSuite = describe "parseSuite" do
    removingComments
    stringParsing
    it "Expression parser smoke test" do
       runParser expression "1" `shouldEqual` Right (Number 1.0)
    it "Expression parser: strip whitespace" do
       runParser expression " 1 " `shouldEqual` Right (Number 1.0)
    it "Parsing smoke test" do
       parse "1" `shouldEqual` Right (Number 1.0)
    it "Parsing string smoke test" do
       parse "\"1\"" `shouldEqual` Right (String "1")
    it "Parsing variable smoke test" do
       parse "foo" `shouldEqual` Right (Ident "foo")

commentWithInlineNewline :: String
commentWithInlineNewline = "foo # new comment \n bar"

removingComments :: forall g m. Monad m => MonadThrow Error g => SpecT g Unit m Unit
removingComments = describe "Test removing comments" do
    it "Test empty string" do
       runParser removeComments "" `shouldEqual` Right ""
    it "Test no comment" do
       runParser removeComments "foo bar" `shouldEqual` Right "foo bar"
    it "Test no comment with string" do
       runParser removeComments "foo \"middle\" bar" `shouldEqual` Right "foo \"middle\" bar"
    it "Test string with hash" do
       runParser removeComments "foo \"abc#middle\" bar" `shouldEqual` Right "foo \"abc#middle\" bar"
    it "Test comment with inline newline" do
       runParser removeComments commentWithInlineNewline `shouldEqual` Right "foo \n bar"

stringParsing :: forall g m. Monad m => MonadThrow Error g => SpecT g Unit m Unit
stringParsing = describe "Test parsing strings" do
    it "Parsing string smoke test" do
       runParser stringExpr "\"1\"" `shouldEqual` Right (String "1")
    it "Test String" do
       (runParser stringExpr "\"Lorem ipsum.\"") `shouldEqual` (Right <<< String $ "Lorem ipsum.")
    it "Parsing variable smoke test" do
       runParser stringExpr "foo" `shouldEqual` Left (ParseError "Expected '\"'.")

numberParsing :: forall g m. Monad m => MonadThrow Error g => SpecT g Unit m Unit
numberParsing = describe "Test parsing strings" do
    it "Test number parser single digit" do
       (runParser numberExpr "3") `shouldEqual` (Right <<< Number $ 3.0)
    it "Test number parser" do
       (runParser numberExpr "14") `shouldEqual` (Right <<< Number $ 14.0)
    it "Test three digit number parser" do
       (runParser numberExpr "789") `shouldEqual` (Right <<< Number $ 789.0)
    it "Test negative number parser" do
       (runParser numberExpr "-37") `shouldEqual` (Right <<< Number $ (-37.0))
    it "Test float" do
       (runParser numberExpr "37.5") `shouldEqual` (Right <<< Number $ (37.5))
