module Interpreter where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Map (Map, lookup, fromFoldable, unions)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Text.Parsing.StringParser.Expr as Op
import Data.Tuple (Tuple(..))
import Data.Array (concat)

import Ast as AST
import Parse (parse, infixOp, prefixOp, postfixOp)
import Symbol (Symbol)

data OpMeta
    = Infix String Op.Assoc
    | Prefix String
    | Postfix String

-- Is Lambda right?
data Operator = Operator Lambda OpMeta

-- TODO Turn this into a record type with ten levels of precedence. Maybe have a locked top and bottom precedence.
type FullOperators = Array (Array Operator)

getExpr :: Operator -> Op.Operator AST.Expression
getExpr (Operator _ opMeta) = case opMeta of
    Infix name assoc-> infixOp assoc name
    Prefix name -> prefixOp name
    Postfix name -> postfixOp name

getFn :: Operator -> Lambda
getFn (Operator fn _) = fn

getOpTable :: FullOperators -> Op.OperatorTable AST.Expression
getOpTable = map (map getExpr)

getFunctions :: FullOperators -> Map String Value
getFunctions fullOps = fromFoldable (map toTup (concat fullOps))
    where
      toTup (Operator fn opMeta) = Tuple (getName opMeta) (FunctionVal fn)
      getName = case _ of
                    Infix name _ -> name
                    Prefix name -> name
                    Postfix name -> name

type Library = Tuple Environment FullOperators

-- Not sure how to handle operators and general defined stuff quite yet.
interpret :: Array Library -> String -> Effect Unit
interpret libs script = do
    let (Tuple env opTable) = importLibs libs script
    let parseResult = parse opTable script
        
    case parseResult of
        Left err -> log $ show err
        Right expr -> eval env expr *> pure unit

-- TODO
-- eval' :: AST.Expression -> Effect 
-- eval' x = case

-- Not sure how to test this.
eval :: Environment -> AST.Expression -> Effect (Either String Value)
eval env (AST.Ident name) = case lookup name (unwrap env).values of
    Nothing -> pure $ Left "That variable is not defined."
    Just val -> pure $ Right val
eval env expr = do
    log "eval not finished yet."
    --pure $ Left "Not implemented"
    pure $ Right (StringVal "foobar")

-- Just using this while I refactor operators again.
tempOps :: Op.OperatorTable AST.Expression
tempOps =
    [ [ infixOp Op.AssocRight "*" ]
    , [ prefixOp "&", postfixOp "!", prefixOp "-" ]
    , [ infixOp Op.AssocRight "+" ]
    , [ infixOp Op.AssocRight "==" ]
    ]

-- Temporarily just joins all the libraries.
-- Clean up types
importLibs :: Array Library -> String -> Tuple Environment (Op.OperatorTable AST.Expression)
importLibs libs script = Tuple (Environment { values: unions (libToMap <$> libs) }) tempOps
    where
      getMap (Environment x) = x
      libToMap (Tuple (Environment a) ops) = unions [a.values, (getFunctions ops)]

-- Should maybe just be Map String Value
newtype Environment = Environment
    { values :: Map String Value
    -- Do we need this?
    -- , symbolCount :: Symbol
    }

derive instance newtypeEnvironment :: Newtype Environment _

data Value
    = StringVal String
    | NumberVal Number
    | TagVal Tag
    | FunctionVal Lambda
    | TagSetVal TagSet
    | ListVal (Array Value)

newtype Tag = Tag
    { symbol :: Symbol
    , name :: String
    , value :: Value
    }

newtype TagSet = TagSet (Map Symbol Value)

newtype Lambda = Lambda
    { parameters :: Array String
    , body :: AST.Expression
    , environment :: Environment
    }
