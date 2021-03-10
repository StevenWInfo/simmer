module Parse where

import Prelude hiding (between)
import Control.Alt ((<|>))
import Text.Parsing.StringParser (Parser, fail, runParser, ParseError, try)
import Text.Parsing.StringParser.CodePoints (string, anyDigit, noneOf, char, eof, whiteSpace, skipSpaces, alphaNum, oneOf)
import Text.Parsing.StringParser.Combinators (many1, many, between, lookAhead, optionMaybe, choice)
import Text.Parsing.StringParser.Expr as Op
import Data.Number (fromString)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String.Yarn (fromChars)
import Data.Foldable (fold, foldl, foldr, elem)
import Data.String.CodeUnits (singleton)
import Data.Either (Either)
import Data.List.NonEmpty (toList)
import Data.Array ((:))
import Control.Monad.Reader.Trans (ReaderT(..), ask, lift, mapReaderT, runReaderT)
import Control.Lazy (class Lazy, fix, defer)

-- import Debug.Trace (spy)

import Ast (Expression(..), Name)

{-
    TODO look into Text.Parsing.StringParser.Expr
    TODO parse importing
    -}

identifyString :: Parser String
identifyString = do
    str <- fromChars <$> between (char '"') (char '"') (many $ noneOf ['"'])
    pure $ "\"" <> str <> "\""

comment :: Parser String
comment = do
    _ <- fromChars <$> between (char '#') end (many $ noneOf ['\n'])
    pure ""
    where
      end = lookAhead $ (singleton <$> char '\n') <|> ((\_ -> "") <$> eof)

removeComments :: Parser String
removeComments = fold <$> many (identifyString <|> comment <|> untilSignificant)
    where untilSignificant = fromChars <$> (many1 $ noneOf ['"', '#'])

-- Just removes comments
parse :: Op.OperatorTable Expression -> String -> Either ParseError Expression
parse opTable source = uncommented >>= runParser ((runReaderT expressionParser) opTable)
    where
      uncommented = runParser removeComments source


-- I guess I've got to have some way of distinguishing between pre-, in-, and post- operators. Either limit to certain operators/characters or by spaces or something.

-- I feel like this might be able to replace most of the expression parser
-- I suppose it can't do things like Call, If, and other things with more than two arguments
-- expressionParser :: Op.OperatorTable Expression -> Parser Expression
-- expressionParser opTable = Op.buildExprParser opTable _?

-- If I don't end up using Op.buildExprParser, then I should make a (ReaderT Op.OperatorTable Parser Expression)

type ParserWithOps = ReaderT (Op.OperatorTable Expression) Parser Expression

{-
--newtype ParserWithOps = ParserWithOps ReaderT (Op.OperatorTable Expression) Parser Expression

instance lazyReaderT :: Lazy ParserWithOps where
    defer f = ReaderT (defer (runReaderT <<< f))
    -}

-- Maybe should use more "try"s
expressionParser :: ParserWithOps
expressionParser = do
    opTable <- ask
    lift skipSpaces
    --_ <- pure ((\_ -> String "") <$> skipSpaces)
    left <- prefixParser
    _ <- lift ((\_ -> String "") <$> skipSpaces)
    maybeExpr <- mapReaderT optionMaybe (infixParser left)
    {-
    skipSpaces
    -}
    pure $ fromMaybe left maybeExpr
    
    -- Can I do operator parsing manually by putting infix and postfix stuff here?
    -- Need to also consider function calls. Might be tricky to deal with as well. Might be simpler if roll into one solution.
        -- If an identifier or paren group are next to something, it is considered called with that thing. Unless it's being passed into something? I guess it just depends.
    where
        toExp = (\ws -> String ws) <$> whiteSpace

{-
prefixParser :: ParserWithOps
prefixParser = fix $ \self -> (lift numberExpr)
    <|> (lift stringExpr)
    <|> parenExpr self
    <|> ifParser
    <|> assignmentExpr
    <|> prefix
    <|> (lift identExpr)
    -}
prefixParser :: ParserWithOps
prefixParser = (lift numberExpr)
    <|> (lift stringExpr)
    <|> parenExpr prefixParser
    <|> ifParser
    <|> assignmentExpr
    <|> prefix
    <|> (lift identExpr)

infixParser :: Expression -> ParserWithOps
infixParser left = (infixOpParser left)
                        -- <|> (postFix opTable)

createInfix :: String -> Op.Operator Expression
createInfix name = Op.Infix op Op.AssocNone
    where
      op = do
             parsedName <- string name
             pure (\l r -> Infix l parsedName r)

defaultOpTable :: Op.OperatorTable Expression
defaultOpTable =
    [ [ createInfix "*" ]
    , [ createInfix "+"]
    ]

skip :: forall a. Parser a -> Parser Unit
skip parser = do
    _ <- parser
    pure unit

-- Why did I want this again?
exprSkip :: Parser Expression -> Parser Expression
exprSkip parser = (\_ -> String "") <$> skip parser

strSkip :: Parser String -> Parser Expression
strSkip parser = (\_ -> String "") <$> parser

-- Probably should just make "-" a prefix operator
toNumber :: Parser Number
toNumber = do
    neg <- string "-" <|> pure ""
    whole <- many1 anyDigit
    dec <- string "." <|> pure ""
    less <- if dec == "." then fromChars <$> (many1 anyDigit) else pure ""
    let numString = neg <> fromChars whole <> dec <> less
    case fromString numString  of
        Just num -> pure num
        Nothing -> fail "Couldn't parse number"

numberExpr :: Parser Expression
numberExpr = Number <$> toNumber

stringExpr :: Parser Expression
stringExpr = do
    _ <- String <$> (string "\"")
    value <- (String <<< fromChars) <$> (many $ noneOf ['"'])
    _ <- String <$> string "\""
    pure value

identExpr :: Parser Expression
identExpr = do
    name <- fromChars <$> many nameCharacters
    if elem name reserved then fail msg else (pure <<< Ident $ name)
    where
      msg = "Using reserved name in unrecognized way"

-- Allow alphanumeric (no just numbers) and certain other characters. Probably "'" and "_"
reserved :: Array String
reserved =
    [ "let"
    , "in"
    --, "\\"
    , "if"
    , "else"
    , "case"
    , "of"
    , "import"
    ]

nameCharacters :: Parser Char
nameCharacters = alphaNum <|> (char '_') <|> (char '\'')

nameParser :: Parser String
nameParser = do
    name <- fromChars <$> many nameCharacters--manyTill anyChar whiteSpace 
    _ <- if elem name reserved then fail "Tried to assign to reserved name" else pure ""
    pure name

-- Can't use `between` with mutual recursion here.
-- Could strings be an operator like this?
parenExpr :: ParserWithOps -> ParserWithOps
parenExpr expParser = do
    _ <- lift $ char '('
    expr <- expParser
    _ <- lift $ char ')'
    pure $ Prefix "(" expr

-- Need to limit to non-reserved things.
assignmentExpr :: ParserWithOps
assignmentExpr = do
    _ <- lift $ try do
       _ <- strSkip $ string "let"
       spaces <- whiteSpace
       if spaces == "" then fail "Not let" else pure ""
    name <- lift nameParser
    lift skipSpaces
    _ <- lift $ string "="
    assignedVal <- expressionParser
    lift skipSpaces
    _ <- lift $ do
       _ <- strSkip $ string "in"
       spaces <- whiteSpace
       if spaces == "" then fail "Not in" else pure ""
    body <- expressionParser
    pure $ Assignment name assignedVal body

reservedOperators :: Array String
reservedOperators =
    [ "("
    ]

opCharacters :: Array Char
opCharacters =
    [ '('
    , ')'
    , '^'
    , '*'
    , '+'
    ]

opCharParser :: Parser Char
opCharParser = oneOf opCharacters

opParser :: Parser Name
opParser = (fromChars <<< toList) <$> (many1 $ opCharParser)

-- TODO Should use the operator table
-- Also, can probably simplify with operator table.
prefix :: ParserWithOps
prefix = do
    name <- lift $ opParser
    expr <- expressionParser
    pure $ Prefix name expr

postfix :: ParserWithOps
postfix = do
    expr <- expressionParser
    name <- lift opParser
    pure $ Postfix expr name

-- Not sure if this actually works.
infixOpParser :: Expression -> ParserWithOps
infixOpParser left = do
    opTable <- ask
    let toInfix op = case op of
                        Op.Infix inOp _ -> Just inOp
                        _ -> Nothing
    let infixOpTable = (map toInfix) <$> opTable
    let folder new accum = maybe accum (\x -> x: accum) new
    let filteredOps = (foldr folder []) <$> infixOpTable
    -- Need to figure out how to actually use it unflattened
    let ops = foldl (<>) [] filteredOps

    lift skipSpaces
    createOp <- lift $ choice ops
    lift skipSpaces
    right <- expressionParser
    pure (createOp left right)

ifParser :: ParserWithOps
ifParser = do
    _ <- lift $ try do
       _ <- strSkip $ string "if"
       spaces <- whiteSpace
       if spaces == "" then fail "Not if" else pure ""
    pred <- expressionParser
    lift skipSpaces
    _ <- lift do
       _ <- strSkip $ string "then"
       spaces <- whiteSpace
       if spaces == "" then fail "Not then" else pure ""
    thn <- expressionParser
    lift skipSpaces
    _ <- lift do
       _ <- strSkip $ string "else"
       spaces <- whiteSpace
       if spaces == "" then fail "Not else" else pure ""
    els <- expressionParser
    pure $ If pred thn els
