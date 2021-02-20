module Ast where

import Data.Maybe (Maybe)

data Expression
    = Ident String
    | Number Number
    | String String
    | Function (Array String) Expression
    | Call Expression Expression
    | Prefix String Expression
    | Infix Expression String Expression
    | PostFix Expression String
    | If Expression Expression (Maybe Expression)
    -- A case expression?
