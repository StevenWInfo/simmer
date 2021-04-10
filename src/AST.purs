module Simmer.AST where

import Prelude
import Data.String.Common (joinWith)

type Name = String

data Expression
    = Ident Name
    | Number Number
    | String String
    | Function (Array Name) Expression
    | Assignment Name Expression Expression
    | EmptyCall Expression
    | Call Expression Expression
    | If Expression Expression Expression
    | List (Array Expression)
    -- A case expression?
    -- TagSet literals too.
    -- Try-catch?
    -- Import

instance showExpression :: Show Expression where
    show e = case e of
                 Ident i -> "Var(" <> show i <> ")"
                 Number n -> "Num(" <> show n <> ")"
                 String s -> "Str(" <> show s <> ")"
                 Function params body -> "Fn " <> show params <> "<" <> show body <> ">"
                 Assignment name expr body -> "Let (" <> show name <> ") Equals (" <> show expr <> ") \n     In (" <> show body <> ")"
                 EmptyCall fn -> "(" <> show fn <> ")!"
                 Call fn input -> "Call<" <> show fn <> ">(" <> show input <> ")"
                 If pred thn els -> "If " <> show pred <> " then " <> show thn <> " else " <> show els
                 List items -> "[ " <> (joinWith ", " (show <$> items)) <> " ]"

derive instance eqExpression :: Eq Expression 

-- Should there be an AST validator? Check structure without evaluating?

-- fn for start of function wouldn't be bad.
