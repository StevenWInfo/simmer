module Test.Interpret where

import Prelude
import Effect.Exception (Error)
import Control.Monad.Error.Class (class MonadThrow)
import Test.Spec (it, pending, describe, SpecT, pending')
import Test.Spec.Assertions (shouldEqual)
import Data.Either (Either(..))

import Ast as AST
import Interpreter as I

{-
interpretSuite :: forall g m. Monad m => MonadThrow Error g => SpecT g Unit m Unit
interpretSuite = describe "Interpreter tests" do
    it "eval smoke test" do
        (I.eval (AST.String "foobar")) `shouldEqual` (pure $ Right (I.String "foobar"))
        -}
