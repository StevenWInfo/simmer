module Test.Builtin where

import Prelude
import Effect (Effect)
import Data.Either (Either(..))
import Effect.Class (liftEffect)

import Test.Spec (it, describe, Spec)
import Test.Spec.Assertions (shouldEqual)

import Builtin (builtinLibrary)
import Builtin.Tags (voidTag)
import Interpret as I

builtinSuite :: Spec Unit
builtinSuite = describe "Builtin functionality tests" do
    builtinSimple

builtinEval :: String -> Effect (Either String I.Value)
builtinEval = I.eval' [ builtinLibrary ]

builtinSimple :: Spec Unit
builtinSimple = describe "Simple eval stuff" do
    it "Test log string" do
       result <- liftEffect $ builtinEval "log \"foo\""
       result `shouldEqual` Right (I.TagVal voidTag)
    it "Test log num" do
       result <- liftEffect $ builtinEval "log 123"
       result `shouldEqual` Right (I.TagVal voidTag)
    it "Test add smoke" do
       result <- liftEffect $ builtinEval "2 + 3"
       result `shouldEqual` Right (I.NumberVal 5.0)
    it "Test multiply smoke" do
       result <- liftEffect $ builtinEval "2 * 3"
       result `shouldEqual` Right (I.NumberVal 6.0)
