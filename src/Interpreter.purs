module Interpreter where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Map (Map, lookup, fromFoldable, unions, member, insert)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Text.Parsing.StringParser.Expr as Op
import Data.Tuple (Tuple(..), snd)
import Data.Array (concat, fold)

import Ast as AST
import Parse (parse, infixOp, prefixOp, postfixOp)
import Symbol (Symbol)

data OpMeta
    = Infix String Op.Assoc
    | Prefix String
    | Postfix String

-- Is Lambda right?
data Operator = Operator Lambda OpMeta

newtype Operators = Operators
    { first :: Array Operator
    , second :: Array Operator
    , third :: Array Operator
    , fourth :: Array Operator
    , fifth :: Array Operator
    , sixth :: Array Operator
    , seventh :: Array Operator
    , eighth :: Array Operator
    , ninth :: Array Operator
    }

-- TODO should probably check for duplicates and decide how order effects it.
-- Or just add namespaced
instance semigroupOperators :: Semigroup Operators where
    append (Operators a) (Operators b) = Operators
        { first: a.first <> b.first
        , second: a.second <> b.second
        , third: a.third <> b.third
        , fourth: a.fourth <> b.fourth
        , fifth: a.fifth <> b.fifth
        , sixth: a.sixth <> b.sixth
        , seventh: a.seventh <> b.seventh
        , eighth: a.eighth <> b.eighth
        , ninth: a.ninth <> b.ninth
        }

instance monoidOperators :: Monoid Operators where
    mempty = emptyOperators

emptyOperators :: Operators
emptyOperators = Operators
    { first: []
    , second: []
    , third: []
    , fourth: []
    , fifth: []
    , sixth: []
    , seventh: []
    , eighth: []
    , ninth: []
    }

toArrays :: Operators -> Array (Array Operator)
toArrays (Operators ops) = 
    [ ops.first
    , ops.second
    , ops.third
    , ops.fourth
    , ops.fifth
    , ops.sixth
    , ops.seventh
    , ops.eighth
    , ops.ninth
    ]

getExpr :: Operator -> Op.Operator AST.Expression
getExpr (Operator _ opMeta) = case opMeta of
    Infix name assoc-> infixOp assoc name
    Prefix name -> prefixOp name
    Postfix name -> postfixOp name

getFn :: Operator -> Lambda
getFn (Operator fn _) = fn

toOpTable :: Operators -> Op.OperatorTable AST.Expression
toOpTable ops = map (map getExpr) (toArrays ops)

getFunctions :: Operators -> Map String Value
getFunctions ops = fromFoldable (map toTup (concat (toArrays ops)))
    where
      toTup (Operator fn opMeta) = Tuple (getName opMeta) (FunctionVal fn)
      getName = case _ of
                    Infix name _ -> name
                    Prefix name -> name
                    Postfix name -> name

type Library = Tuple Environment Operators

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
eval env (AST.Number num) = pure <<< Right $ NumberVal num
eval env (AST.String str) = pure <<< Right $ StringVal str
eval env (AST.Assignment name exprA exprB) =
    if name `member` envMap
        then (pure $ Left alreadyDefinedMsg)
        else evalAssignment
    where
      envMap = (_.values $ unwrap env)
      alreadyDefinedMsg = "Name " <> name <> " has already been defined."
      evalAssignment = do
         resultA <- eval env exprA
         case resultA of
             Left err -> pure $ Left err
             Right valA -> eval (Environment { values: insert name valA envMap }) exprB

eval env expr = do
    log "eval not finished yet."
    --pure $ Left "Not implemented"
    pure $ Right (StringVal "foobar")

-- Temporarily just joins all the libraries.
-- Clean up types
-- This should be more complicated to namespace things properly and whatnot.
importLibs :: Array Library -> String -> Tuple Environment (Op.OperatorTable AST.Expression)
importLibs libs script = Tuple newEnv newOps
    where
      getMap (Environment x) = x
      libToMap (Tuple (Environment a) opsToMap) = unions [a.values, (getFunctions opsToMap)]
      newEnv = Environment { values: unions (libToMap <$> libs) }
      newOps = toOpTable (fold (snd <$> libs))

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
