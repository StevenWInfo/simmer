module Ast where

import Prelude

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
    | If Expression Expression Expression
    -- A case expression?

instance showExpression :: Show Expression where
    show e = case e of
                 Ident i -> "Var(" <> show i <> ")"
                 Number n -> "Num(" <> show n <> ")"
                 String s -> "Str(" <> show s <> ")"
                 Function params body -> "Fn" <> show params <> "<" <> show body <> ">"
                 Assignment i body -> "Let(" <> show i <> ")In(" <> show body <> ")"
                 Call fn input -> "AppliedFn<" <> show fn <> ">(" <> show input <> ")"
                 Prefix op exp -> show op <> show exp
                 Infix pre op post -> show pre <> " " <> show op <> " " <> show post
                 PostFix exp op -> show exp <> show op
                 If pred thn els -> "If " <> show pred <> " then " <> show thn <> " else " <> show els

derive instance eqExpression :: Eq Expression 
