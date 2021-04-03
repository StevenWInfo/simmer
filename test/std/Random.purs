module Test.Std.Random where

import Prelude
import Effect (Effect)
import Data.Either (Either)
import Effect.Class (liftEffect)
import Test.Spec (it, describe, Spec)
import Test.Spec.Assertions (shouldNotEqual)

import Builtin as B
import Std.Random as R
import Interpret as I

suite :: Spec Unit
suite = describe "std library Random tests" do
    randomSimple

eval :: String -> Effect (Either String I.Value)
eval = I.eval' [ B.builtinLibrary, R.library ]

randomSimple :: Spec Unit
randomSimple = describe "Simple random stuff" do
    -- Technically non-deterministic, but very unlikely to fail. Not sure how to test random stuff.
    it "Test random function" do
       result1 <- liftEffect $ eval "random!"
       result2 <- liftEffect $ eval "random!"
       result1 `shouldNotEqual` result2
