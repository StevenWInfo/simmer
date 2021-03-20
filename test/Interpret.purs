module Test.Interpret where

import Prelude
import Effect (Effect)
import Test.Spec (it, describe, Spec, pending')
import Test.Spec.Assertions (shouldEqual)
import Data.Either (Either(..))
import Data.Map (empty, singleton)
import Data.Array (uncons)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Class (liftEffect)
import Effect.Aff (Aff)

import Ast as AST
import Interpret as I

interpretSuite :: Spec Unit
interpretSuite = describe "Interpreter tests" do
    evalSimple
    parseAndEval

emptyEnviron :: I.Environment
emptyEnviron  = I.Environment { values: empty }

simpleFn :: I.Fn
simpleFn = I.Lambda
    { parameters: [ "x" ]
    , body: AST.Ident "x"
    , environment: emptyEnviron
    }

-- TODO This obviously shows that creating foreign functions need to be simplified somehow.
-- One thing that could help would be a type class which converts things to I.Values.
-- Could also just have analogous types to Rough types that are members of the class that easily conver. E.g. Purescript Number to Rough Number, etc.
-- Could also probably use phantom types.
-- This is still probably going to be difficult. Would a scripting and App/Lib language developed together be significantly better?
twoParam :: I.Fn
twoParam = I.Foreign handleMaybe
    where
      handleParams :: Array I.Value -> Maybe (Effect (Either String I.Value))
      handleParams params = do
         f <- uncons params
         let first = f.head
         g <- uncons f.tail
         let second = g.head
         let applied = (fn first second) :: Effect (Either String I.Value)
         if g.tail /= [] then Nothing else (Just applied)
      tooMany :: Effect (Either String I.Value)
      tooMany = do
         pure <<< Left $ "Too many parameters"
      handleMaybe :: Array I.Value -> Effect (Either String I.Value)
      handleMaybe params = fromMaybe tooMany (handleParams params)
      fn :: I.Value -> I.Value -> Effect (Either String I.Value)
      fn (I.NumberVal l) (I.NumberVal r) = do
       pure <<< Right <<< I.NumberVal$ l + r
      fn _ _ = do
       pure <<< Left $ "Expected two numbers."

evalSimple :: Spec Unit
evalSimple = describe "Simple eval stuff" do
    it "Test eval ident smoke" do
       result <- (liftEffect $ I.eval (I.Environment { values: singleton "foo" (I.StringVal "bar") }) (AST.Ident "foo")) :: Aff (Either String I.Value)
       result `shouldEqual` Right (I.StringVal "bar")
    it "Test eval num smoke" do
       result <- (liftEffect $ I.eval emptyEnviron (AST.Number 123.0)) :: Aff (Either String I.Value)
       result `shouldEqual` Right (I.NumberVal 123.0)
    it "Test eval string smoke" do
       result <- (liftEffect $ I.eval emptyEnviron (AST.String "Foo")) :: Aff (Either String I.Value)
       result `shouldEqual` Right (I.StringVal "Foo")
    it "Test eval assign smoke" do
       result <- (liftEffect $ I.eval emptyEnviron (AST.Assignment "Foo" (AST.Number 123.0) (AST.Number 789.0))) :: Aff (Either String I.Value)
       result `shouldEqual` Right (I.NumberVal 789.0)
    it "Test eval prefix smoke" do
       result <- (liftEffect $ I.eval (I.Environment { values: singleton "*" (I.FunctionVal simpleFn) }) (AST.Call (AST.Ident "*") (AST.String "bar")))
       result `shouldEqual` Right (I.StringVal "bar")
    it "Test eval postfix smoke" do
       result <- (liftEffect $ I.eval (I.Environment { values: singleton "!" (I.FunctionVal simpleFn) }) (AST.Call (AST.Ident "!") (AST.String "bar")))
       result `shouldEqual` Right (I.StringVal "bar")
    it "Test eval infix smoke" do
       result <- (liftEffect $ I.eval (I.Environment { values: singleton "+" (I.FunctionVal twoParam) }) (AST.Call (AST.Call (AST.Ident "+") (AST.Number 2.0)) (AST.Number 3.0)))
       result `shouldEqual` Right (I.NumberVal 5.0)
    it "Test eval with too many parameters" do
       result <- (liftEffect $ I.eval (I.Environment { values: singleton "+" (I.FunctionVal twoParam) }) (AST.Call (AST.Call (AST.Call (AST.Ident "+") (AST.Number 2.0)) (AST.Number 3.0)) (AST.Number 3.0)))
       result `shouldEqual` (Left "Too many parameters")

parseAndEval :: Spec Unit
parseAndEval = describe "Parsing then evaluating" do
    pending' "Test eval ident smoke" do
       result <- (liftEffect $ I.eval (I.Environment { values: singleton "foo" (I.StringVal "bar") }) (AST.Ident "foo")) :: Aff (Either String I.Value)
       result `shouldEqual` Right (I.StringVal "bar")
