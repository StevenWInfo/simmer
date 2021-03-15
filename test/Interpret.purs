module Test.Interpret where

import Prelude
import Test.Spec (it, describe, Spec)
import Test.Spec.Assertions (shouldEqual)
import Data.Either (Either(..))
import Data.Map (empty)
import Effect.Class (liftEffect)
import Effect.Aff (Aff)

import Ast as AST
import Interpret as I

interpretSuite :: Spec Unit
interpretSuite = describe "Interpreter tests" do
    evalSimple

emptyEnviron :: I.Environment
emptyEnviron  = I.Environment { values: empty }

evalSimple :: Spec Unit
evalSimple = describe "Test removing comments" do
    it "Test eval num smoke" do
       result <- (liftEffect $ I.eval emptyEnviron (AST.Number 123.0)) :: Aff (Either String I.Value)
       result `shouldEqual` Right (I.NumberVal 123.0)
