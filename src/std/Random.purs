module Std.Random where

import Prelude
import Effect (Effect)
import Effect.Random (random)
import Data.Map (Map, fromFoldable)
import Data.Tuple (Tuple(..))
import Data.Newtype (over)
import Data.Either (Either(..))

import Interpret as I
import Interface (convertFn)

simmerRandom :: I.TempForeignFn
simmerRandom = convertFn r
    where
      r :: Effect (Either String Number)
      r = Right <$> random

functions :: Map String I.Value
functions = (I.FunctionVal <<< I.Foreign) <$> (fromFoldable fns)
    where
      fns =
          [ Tuple "random" $ simmerRandom
          ]

env :: I.Environment
env = I.Environment { values: functions }

ops :: I.Operators
ops = over I.Operators replace I.emptyOperators
    where
      replace empty = empty -- { }

library :: I.Library
library = Tuple env ops
