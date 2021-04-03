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
parse, identExpr, assignmentExpr, ifParser, infixOp,
    prefixOp, postfixOp)

-- TODO put in some tests where parsers should fail.
parseSuite :: forall g m. Monad m => MonadThrow Error g => SpecT g Unit m Unit
parseSuite = describe "parseSuite" do
    removingComments
    stringParsing
    numberParsing
    assignmentParsing
    identParsing
    ifParsing
    generalParsing
    longerParsing

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
    -- TODO Not sure why this one isn't passing.
    pending' "Test assignment smoke" do
       (runParser (assignmentExpr identExpr) "let foo = bar in baz") `shouldEqual` Right (Assignment "foo" (Ident "bar") (Ident "baz"))
    it "Test reserved name" do
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

ifParsing :: forall g m. Monad m => MonadThrow Error g => SpecT g Unit m Unit
ifParsing = describe "Test parsing prefixes" do
    it "Test if smoke" do
       runParser (ifParser identExpr) "if true then foo else bar" `shouldEqual` Right (If (Ident "true") (Ident "foo") (Ident "bar"))

ops :: Op.OperatorTable Expression
ops =
    [ [ infixOp Op.AssocRight "*" ]
    , [ prefixOp "&", postfixOp "!", prefixOp "-" ]
    , [ infixOp Op.AssocRight "+" ]
    , [ infixOp Op.AssocRight "==" ]
    ]

generalParsing :: forall g m. Monad m => MonadThrow Error g => SpecT g Unit m Unit
generalParsing = describe "Test general parsing" do
    it "Expression parser smoke test" do
       runParser (expressionParser []) "1" `shouldEqual` Right (Number 1.0)
    it "Expression parser: strip whitespace" do
       runParser (expressionParser []) " 1 " `shouldEqual` Right (Number 1.0)
    it "Parsing smoke test" do
       parse ops "1" `shouldEqual` Right (Number 1.0)
    it "Parsing string smoke test" do
       parse ops "\"1\"" `shouldEqual` Right (String "1")
    it "Parsing variable smoke test" do
       parse ops "foo" `shouldEqual` Right (Ident "foo")
    -- Should this pass or fail?
    pending' "Test ident start with number" do
       parse ops "37foo" `shouldEqual` Right (Ident "37foo")
    it "Parsing negative test" do
       parse ops "-1" `shouldEqual` Right (Call (Ident "-") (Number (1.0)))
    it "Test assignment smoke" do
       parse ops "let foo = 123 in foo" `shouldEqual` Right (Assignment "foo" (Number 123.0) (Ident "foo"))
    it "Test assignment in assignment" do
       parse ops "let foo = let bar = 123 in bar in foo" `shouldEqual` Right (Assignment "foo" (Assignment "bar" (Number 123.0) (Ident "bar")) (Ident "foo"))
    it "Test paren smoke" do
       parse ops "(123)" `shouldEqual` Right (Call (Ident "(") (Number 123.0))
    it "Test paren in paren" do
       parse [] "((123))" `shouldEqual` Right (Call (Ident "(") (Call (Ident "(") (Number 123.0)))
    it "Test nested assignment" do
       parse [] "let foo = (let bar = 123 in bar) in foo" `shouldEqual` Right (Assignment "foo" (Call (Ident "(") (Assignment "bar" (Number 123.0) (Ident "bar"))) (Ident "foo"))
    it "Test if smoke" do
       parse ops "if true then 123 else \"abc\"" `shouldEqual` Right (If (Ident "true") (Number 123.0) (String "abc"))
    it "Test infix operator" do
       parse ops "123 + 456" `shouldEqual` Right (Call (Call (Ident "+") (Number 123.0)) (Number 456.0))
    it "Test prefix operator" do
       parse ops "&foo" `shouldEqual` Right (Call (Ident "&") (Ident "foo"))
    it "Test postfix operator" do
       parse ops "foo!" `shouldEqual` Right (Call (Ident "!") (Ident "foo"))
    it "Testing operator precedence" do
       parse ops "1 + 2 * 3" `shouldEqual` Right (Call (Call (Ident "+") (Number 1.0)) (Call (Call (Ident "*") (Number 2.0)) (Number 3.0)))
    it "Testing operator precedence other way" do
       parse ops "1 * 2 + 3" `shouldEqual` Right (Call (Call (Ident "+") (Call (Call (Ident "*") (Number 1.0)) (Number 2.0))) (Number 3.0))
    it "Testing embedded ifs" do
       parse ops "if true then if false then 123 else 789 else (if true then \"foo\" else \"bar\")" `shouldEqual` Right (
           If (Ident "true") (If (Ident "false") (Number 123.0) (Number 789.0)) (Call (Ident "(") (If (Ident "true") (String "foo") (String "bar")))
           )
    it "Test if in if" do
       parse ops "if true then foo else if false then bar else baz" `shouldEqual` Right (If (Ident "true") (Ident "foo") (If (Ident "false") (Ident "bar") (Ident "baz")))
    it "Test function call" do
       parse ops "foo 1" `shouldEqual` Right (Call (Ident "foo") (Number 1.0))
    it "Test function call two" do
       parse ops "foo 1 2" `shouldEqual` Right (Call (Call (Ident "foo") (Number 1.0)) (Number 2.0))
    it "Test reserved name" do
       parse ops "let if = 123 in 789" `shouldEqual` Left (ParseError "Tried to assign to reserved name")
    it "Test lambda smoke" do
       parse ops "\\a -> a" `shouldEqual` Right (Function ["a"] (Ident "a"))
    it "Test lambda two params" do
       parse ops "\\a b -> a" `shouldEqual` Right (Function ["a", "b"] (Ident "a"))
    it "Test function call" do
       parse ops "log \"foo\"" `shouldEqual` Right (Call (Ident "log") (String "foo"))
    it "Test function call" do
       parse ops "log 123" `shouldEqual` Right (Call (Ident "log") (Number 123.0))
    -- It would be nice if the error actually said the character.
    it "Test function call number" do
       parse ops "log @123" `shouldEqual` Left (ParseError "Could not match character '\\''")
    it "Test lists" do
       parse ops "[2, 3, 5]" `shouldEqual` Right (List [Number 2.0, Number 3.0, Number 5.0])

longerExample :: String
longerExample = """
    let foo = 1 in
    let bar = 2 in
    let baz = if foo + bar == 3 then
        "abc"
        else
        "xyz"
    in
        baz
    """

longerExampleAst :: Expression
longerExampleAst = Assignment "foo" (Number 1.0)
    (Assignment "bar" (Number 2.0)
        (Assignment "baz"
            (If
                (Call (Call (Ident "==") (Call (Call (Ident "+") (Ident "foo")) (Ident "bar"))) (Number 3.0))
                (String "abc")
                (String "xyz"))
            (Ident "baz")
        )
    )

longerParsing :: forall g m. Monad m => MonadThrow Error g => SpecT g Unit m Unit
longerParsing = describe "Test longer parsing" do
    it "Testing a longer expression" do
       parse ops longerExample `shouldEqual` Right longerExampleAst
