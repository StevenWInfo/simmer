module Test.Interpret where

import Prelude
import Test.Spec (it, describe, Spec)
import Test.Spec.Assertions (shouldEqual)
import Data.Either (Either(..))
import Data.Map (empty, singleton)
import Effect.Class (liftEffect)
import Effect.Aff (Aff)

import Ast as AST
import Interpret as I

interpretSuite :: Spec Unit
interpretSuite = describe "Interpreter tests" do
    evalSimple

emptyEnviron :: I.Environment
emptyEnviron  = I.Environment { values: empty }

simpleFn :: I.Lambda
simpleFn = I.Lambda
    { parameters: [ "x" ]
    , body: AST.Ident "x"
    , environment: emptyEnviron
    }

evalSimple :: Spec Unit
evalSimple = describe "Test removing comments" do
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
    it "Test eval ident smoke" do
       result <- (liftEffect $ I.eval (I.Environment { values: singleton "*" (I.FunctionVal simpleFn) }) (AST.Prefix "*" (AST.String "bar")))
       result `shouldEqual` Right (I.StringVal "bar")
