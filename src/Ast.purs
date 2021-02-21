module Ast where

import Prelude

import Data.Maybe (Maybe)

type Name = String

data Expression
    = Ident Name
    | Number Number
    | String String
    | Function (Array String) Expression
    | Assignment String Expression
    | Call Expression (Array Expression)
    | Prefix String Expression
    | Infix Expression String Expression
    | PostFix Expression String
    | If Expression Expression (Maybe Expression)
    -- A case expression?

instance showExpression :: Show Expression where
    show e = case e of
                 Ident i -> "Var(" <> show i <> ")"
                 Number n -> "Num(" <> show n <> ")"
                 String s -> "Str(" <> show s <> ")"
                 Function params body -> "Fn" <> show params <> "<" <> show body <> ">"
                 Assignment i body -> "Let(" <> show i <> ")In(" <> show body <> ")"
                 Call fn input -> "AppliedFn<" <> show fn <> ">(" <> show input <> ")"
                 -- TODO
                 Prefix _ _ -> "foo"
                 Infix _ _ _ -> "foo"
                 PostFix _ _ -> "foo"
                 If _ _ _ -> "foo"

derive instance eqExpression :: Eq Expression 
