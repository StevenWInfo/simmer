module Interpreter where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Map (Map)

import Ast (Expression)
import Symbol (Symbol)

interpret :: Expression -> Effect Unit
interpret expr = do
    log "Interpreter not implemented yet."

newtype Environment = Environment
    { values :: Map String Value
    , symbolCount :: Symbol
    }

data Value
    = String String
    | Number Number
    | TagVal Tag
    | FunctionVal Lambda
    | TagSetVal TagSet
    | List (Array Value)

newtype Tag = Tag
    { symbol :: Symbol
    , name :: String
    , value :: Value
    }

newtype TagSet = TagSet (Map Symbol Value)

newtype Lambda = Lambda
    { parameters :: Array String
    , body :: Expression
    , environment :: Environment
    }
