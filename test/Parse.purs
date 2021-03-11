module Test.Parse where

import Prelude
import Effect.Exception (Error)
import Control.Monad.Error.Class (class MonadThrow)
import Data.Either (Either(..))
import Text.Parsing.StringParser (runParser, ParseError(..))
import Text.Parsing.StringParser.Expr as Op
import Test.Spec (it, pending', describe, SpecT)
import Test.Spec.Assertions (shouldEqual)

import Ast (Expression(..))
import Parse (numberExpr, stringExpr, removeComments, expressionParser,
parse, identExpr, assignmentExpr, prefix, postfix, infixParser, ifParser, infixOp,
    prefixOp)

-- TODO put in some tests where parsers should fail.
parseSuite :: forall g m. Monad m => MonadThrow Error g => SpecT g Unit m Unit
parseSuite = describe "parseSuite" do
    removingComments
    stringParsing
    numberParsing
    assignmentParsing
    identParsing
    prefixParsing
    postfixParsing
    infixParsing
    ifParsing
    generalParsing

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

assignmentParsing :: forall g m. Monad m => MonadThrow Error g => SpecT g Unit m Unit
assignmentParsing = describe "Test parsing assignment" do
    it "Test assignment smoke" do
       (runParser (assignmentExpr numberExpr) "let foo = 123 in 789") `shouldEqual` Right (Assignment "foo" (Number 123.0) (Number 789.0))
    it "Test assignment smoke" do
       runParser (assignmentExpr numberExpr) "let if = 123 in 789" `shouldEqual` Left (ParseError "Tried to assign to reserved name")

identParsing :: forall g m. Monad m => MonadThrow Error g => SpecT g Unit m Unit
identParsing = describe "Test parsing variables" do
    it "Test ident smoke" do
       (runParser identExpr "foo") `shouldEqual` (Right $ Ident "foo")
    it "Test ident with number" do
       (runParser identExpr "foo37") `shouldEqual` (Right $ Ident "foo37")
    -- Should this pass or fail?
    pending' "Test ident start with number" do
       (runParser identExpr "37foo") `shouldEqual` (Right $ Ident "37foo")

prefixParsing :: forall g m. Monad m => MonadThrow Error g => SpecT g Unit m Unit
prefixParsing = describe "Test parsing prefixes" do
    it "Test prefix smoke" do
       -- Not sure I actually even want this to be a prefix operator.
       runParser (prefix [[ prefixOp "^" ]] numberExpr) "^123" `shouldEqual` Right (Prefix "^" (Number 123.0))

postfixParsing :: forall g m. Monad m => MonadThrow Error g => SpecT g Unit m Unit
postfixParsing = describe "Test parsing prefixes" do
    pending' "Test postfix smoke" do
       -- Not sure I actually even want this to be a postfix operator.
       runParser (postfix stringExpr) "foo*" `shouldEqual` Right (Postfix (Ident "foo") "*")

infixParsing :: forall g m. Monad m => MonadThrow Error g => SpecT g Unit m Unit
infixParsing = describe "Test parsing prefixes" do
    it "Test infix smoke" do
       let op = [[infixOp Op.AssocNone "+"]]
       runParser (infixParser op numberExpr (Number 789.0)) "+ 123" `shouldEqual` Right (Infix (Number 789.0) "+" (Number 123.0))

ifParsing :: forall g m. Monad m => MonadThrow Error g => SpecT g Unit m Unit
ifParsing = describe "Test parsing prefixes" do
    it "Test if smoke" do
       runParser (ifParser identExpr) "if true then foo else bar" `shouldEqual` Right (If (Ident "true") (Ident "foo") (Ident "bar"))
    pending' "Test if in if" do
       runParser (ifParser identExpr) "if true then foo else if false then bar else baz" `shouldEqual` Right (If (Ident "true") (Ident "foo") (If (Ident "false") (Ident "bar") (Ident "baz")))

ops :: Op.OperatorTable Expression
ops =
    [ [ prefixOp "!" ]
    , [ infixOp Op.AssocLeft "*" ]
    , [ infixOp Op.AssocLeft "+" ]
    ]

generalParsing :: forall g m. Monad m => MonadThrow Error g => SpecT g Unit m Unit
generalParsing = describe "Test general parsing" do
    it "Expression parser smoke test" do
       runParser (expressionParser []) "1" `shouldEqual` Right (Number 1.0)
    it "Expression parser: strip whitespace" do
       runParser (expressionParser []) " 1 " `shouldEqual` Right (Number 1.0)
    it "Parsing smoke test" do
       parse [] "1" `shouldEqual` Right (Number 1.0)
    it "Parsing string smoke test" do
       parse [] "\"1\"" `shouldEqual` Right (String "1")
    it "Parsing variable smoke test" do
       parse [] "foo" `shouldEqual` Right (Ident "foo")
    -- Should this pass or fail?
    pending' "Test ident start with number" do
       parse [] "37foo" `shouldEqual` Right (Ident "37foo")
    it "Test assignment smoke" do
       parse [] "let foo = 123 in foo" `shouldEqual` Right (Assignment "foo" (Number 123.0) (Ident "foo"))
    it "Test paren smoke" do
       parse [] "(123)" `shouldEqual` Right (Prefix "(" (Number 123.0))
    it "Test assignment smoke" do
       parse [] "let foo = (let bar = 123 in bar) in foo" `shouldEqual` Right (Assignment "foo" (Prefix "(" (Assignment "bar" (Number 123.0) (Ident "bar"))) (Ident "foo"))
    it "Test if smoke" do
       parse [] "if true then 123 else \"abc\"" `shouldEqual` Right (If (Ident "true") (Number 123.0) (String "abc"))
    it "Test infix operator" do
       parse ops "123 + 456" `shouldEqual` Right (Infix (Number 123.0) "+" (Number 456.0))
    it "Test prefix operator" do
       parse ops "!foo" `shouldEqual` Right (Prefix "!" (Ident "foo"))
