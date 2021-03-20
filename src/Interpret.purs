module Interpret where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Map (Map, lookup, fromFoldable, unions, member, insert, keys, intersection, union)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap)
import Text.Parsing.StringParser.Expr as Op
import Data.Tuple (Tuple(..), snd)
import Data.Array (concat, fold, length, zip)
import Data.Traversable (sequence)
import Data.String.Common (joinWith)
import Data.Set (toUnfoldable)

import Ast as AST
import Parse (parse, infixOp, prefixOp, postfixOp)
import Symbol (Symbol)

data OpMeta
    = Infix String Op.Assoc
    | Prefix String
    | Postfix String

data Operator = Operator Fn OpMeta

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

toSuper :: Operators -> SuperOperators
toSuper regular = SuperOperators regular specialOperators

data SuperOperators = SuperOperators Operators 
    { top :: Array Operator
    , bottom :: Array Operator
    }

instance semigroupSuperOperators :: Semigroup SuperOperators where
    append (SuperOperators ll lr) (SuperOperators rl rr) = SuperOperators (ll <> rl) specialOperators

instance monoidSuperOperators :: Monoid SuperOperators where
    mempty = emptySuperOperators

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

specialOperators :: { top :: Array Operator, bottom :: Array Operator }
specialOperators = 
    { top: []
    , bottom: []
    }

emptySuperOperators :: SuperOperators
emptySuperOperators = SuperOperators emptyOperators specialOperators

toArrays :: SuperOperators -> Array (Array Operator)
toArrays (SuperOperators (Operators regular) super) = 
    [ super.top
    , regular.first
    , regular.second
    , regular.third
    , regular.fourth
    , regular.fifth
    , regular.sixth
    , regular.seventh
    , regular.eighth
    , regular.ninth
    , super.bottom
    ]

getExpr :: Operator -> Op.Operator AST.Expression
getExpr (Operator _ opMeta) = case opMeta of
    Infix name assoc-> infixOp assoc name
    Prefix name -> prefixOp name
    Postfix name -> postfixOp name

getFn :: Operator -> Fn
getFn (Operator fn _) = fn

toOpTable :: SuperOperators -> Op.OperatorTable AST.Expression
toOpTable ops = map (map getExpr) (toArrays ops)

getFunctions :: SuperOperators -> Map String Value
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

eval env (AST.Prefix name expr) = callNamedValue env name [ expr ]
eval env (AST.Infix exprL name exprR) = callNamedValue env name [ exprL, exprR ]
eval env (AST.Postfix expr name) = callNamedValue env name [ expr ]

eval env expr = do
    log "eval not finished yet."
    --pure $ Left "Not implemented"
    pure $ Right (StringVal "foobar")

callNamedValue :: Environment -> String -> Array AST.Expression -> Effect (Either String Value)
callNamedValue env valName params = maybe default callPrefix possiblePrefix
    where
      default = pure $ Left ("Couldn't find defined function " <> valName)
      callPrefix prefix = callValue env prefix params
      possiblePrefix = lookup valName (_.values $ unwrap env)

callValue :: Environment -> Value -> Array AST.Expression -> Effect (Either String Value)
callValue env (FunctionVal fn) exprs = callFn env fn exprs
callValue _ _ _ = pure $ Left "Called a value which isn't a function."

-- TODO currying
callFn :: Environment -> Fn -> Array AST.Expression -> Effect (Either String Value)
callFn env (Lambda lambda) exprs = if (length exprs) /= (length lambda.parameters) then mismatchedResult else do
    allResults <- sequence $ (eval env) <$> exprs
    -- I think this just gets first error. May want to get more.
    let possibleResults = sequence allResults
    case possibleResults of
        Left msg -> pure $ Left msg
        Right inParams -> apply inParams
    where
      mismatchedResult = pure $ Left "Number of expected and input parameters don't match"
      apply inParams = do
         let zipped = zip lambda.parameters inParams
         let paramMap = fromFoldable zipped
         let possibleExpanded = addScope env paramMap
         case possibleExpanded of
             Right newEnv -> eval newEnv lambda.body
             Left msg -> pure $ Left msg

callFn env (Foreign externalFn) exprs = do
    allResults <- sequence $ (eval env) <$> exprs
    let possibleResults = sequence allResults
    case possibleResults of
        Left msg -> pure $ Left msg
        Right inParams -> externalFn inParams

-- Temporarily just joins all the libraries.
-- Clean up types
-- This should be more complicated to namespace things properly and whatnot.
importLibs :: Array Library -> String -> Tuple Environment (Op.OperatorTable AST.Expression)
importLibs libs script = Tuple newEnv newOps
    where
      getMap (Environment x) = x
      libToMap (Tuple (Environment a) opsToMap) = unions [a.values, (getFunctions (toSuper opsToMap))]
      newEnv = Environment { values: unions (libToMap <$> libs) }
      newOps = toOpTable (fold ((toSuper <<< snd) <$> libs))

-- Should maybe just be Map String Value
-- I suppose could hold operators separately which might be helpful. Combine with library?
newtype Environment = Environment
    { values :: Map String Value
    -- Do we need this?
    -- , symbolCount :: Symbol
    }

derive instance newtypeEnvironment :: Newtype Environment _

derive instance eqEnvironment :: Eq Environment

addScope :: Environment -> Map AST.Name Value -> Either String Environment
addScope (Environment env) defined = if (length clashes) /= 0 then clashMsg clashes else Right $ Environment { values: env.values `union` defined }
    where
      clashMsg names = Left $ "Names [ " <> (joinWith ", " names) <> " ] have already been defined."
      clashes = toUnfoldable <<< keys $ intersection env.values defined

-- Should maybe remove "val" from ends. They're going to be used more than AST versions.
data Value
    = StringVal String
    | NumberVal Number
    | TagVal Tag
    | FunctionVal Fn
    | TagSetVal TagSet
    | ListVal (Array Value)

instance showValue :: Show Value where
    show (StringVal s) = "Str(" <> s <> ")"
    show (NumberVal n) = "Num(" <> show n <> ")"
    show (TagVal t) = show t
    show (FunctionVal l) = show l
    show (TagSetVal ts) = show ts
    show (ListVal l) = show l

derive instance eqValue :: Eq Value

newtype Tag = Tag
    { symbol :: Symbol
    , name :: AST.Name
    , value :: Value
    }

derive instance eqTag :: Eq Tag

instance showTag :: Show Tag where
    show (Tag t) = "(" <> t.name <> ": " <> show t.value <> ")"

newtype TagSet = TagSet (Map Symbol Value)

derive newtype instance showTagSet :: Show TagSet
derive instance eqTagSet :: Eq TagSet

-- Uses Array for parameters, but could potentially make polyvariadic (at least in Haskell): https://wiki.haskell.org/Varargs
type TempForeignFn = Array Value -> Effect (Either String Value)

data ForeignFn = Param Value ForeignFn | Final (Value -> Effect (Either String Value))

data Fn = Foreign TempForeignFn | Lambda
                              { parameters :: Array String
                              , body :: AST.Expression
                              , environment :: Environment
                              }

-- TODO At least have some sort of hash or something. Want something better.
instance showFn :: Show Fn where
    show (Lambda l) = "fn(" <> fold l.parameters <> ")"
    show (Foreign l) = "fn(EXTERNAL)"

-- TODO This is only for testing. Don't export for general use.
--derive instance eqLambda :: Eq Fn
instance eqFn :: Eq Fn where
    eq (Lambda l) (Lambda r) = l == r
    eq l r = false
