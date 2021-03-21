module Builtin where

import Data.Map (fromFoldable)
import Data.Newtype (over)
import Data.Tuple (Tuple(..))

import Interpret as I

builtinEnv :: I.Environment
builtinEnv = I.Environment
    { values: fromFoldable
    [
    ]
    }

builtinOps :: I.Operators
builtinOps = over I.Operators replace I.emptyOperators
    where
      replace empty = empty { first = [ ] }

builtinLibrary :: I.Library
builtinLibrary = Tuple builtinEnv builtinOps
