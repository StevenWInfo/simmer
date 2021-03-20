module Ast where

import Prelude
-- import Data.String.Common (joinWith)

type Name = String

data Expression
    = Ident Name
    | Number Number
    | String String
    | Function (Array Name) Expression
    | Assignment Name Expression Expression
    -- Body and a single param
    | Call Expression Expression
    | Prefix Name Expression
    | Infix Expression Name Expression -- TODO Can I get rid of these operators and just replace with function calls?
    | Postfix Expression Name
    | If Expression Expression Expression
    -- | List (Array Expression)
    -- A case expression?
    -- Want list and TagSet literals too.
    -- Try-catch?
    -- Import

instance showExpression :: Show Expression where
    show e = case e of
                 Ident i -> "Var(" <> show i <> ")"
                 Number n -> "Num(" <> show n <> ")"
                 String s -> "Str(" <> show s <> ")"
                 Function params body -> "Fn " <> show params <> "<" <> show body <> ">"
                 Assignment name expr body -> "Let (" <> show name <> ") Equals (" <> show expr <> ") \n     In (" <> show body <> ")"
                 Call fn input -> "Call<" <> show fn <> ">(" <> show input <> ")"
                 Prefix op exp -> "(" <> show op <> show exp <> ")"
                 Infix pre op post -> "(" <> show pre <> " " <> show op <> " " <> show post <> ")"
                 Postfix exp op -> show exp <> show op
                 If pred thn els -> "If " <> show pred <> " then " <> show thn <> " else " <> show els
                 -- List items -> "[ " <> (joinWith ", " (show <$> items)) <> " ]"

derive instance eqExpression :: Eq Expression 

-- Should there be an AST validator? Check structure without evaluating?

-- fn for start of function wouldn't be bad.
