module Parse where

import Prelude hiding (between)
import Control.Alt ((<|>))
import Text.Parsing.StringParser (Parser, fail, runParser, ParseError, try)
import Text.Parsing.StringParser.CodePoints (string, anyDigit, noneOf, char, eof, whiteSpace, skipSpaces, alphaNum)
import Text.Parsing.StringParser.Combinators (many1, many, between, lookAhead, choice)
import Text.Parsing.StringParser.Expr as Op
import Data.Number (fromString)
import Data.Maybe (Maybe(..), maybe)
import Data.String.Yarn (fromChars)
import Data.Foldable (fold, foldl, foldr, elem)
import Data.String.CodeUnits (singleton)
import Data.Either (Either)
import Data.Array ((:), catMaybes)
import Control.Lazy (fix)

-- import Debug.Trace (spy)

import Ast (Expression(..), Name)

{-
    TODO look into Text.Parsing.StringParser.Expr
    TODO parse importing

    I'd like to clean up a lot of this code.
    I'd like to use ReaderT to pass around the OperatorTable, but I couldn't figure out how to make a newtype of it an instance of the Lazy typeclass for `fix` to have mutual references.
    Can simplify it a lot with OperatorTable. Might even be able to do most of it with buildExprParser.
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
parse opTable source = uncommented >>= runParser (expressionParser opTable)
    where
      uncommented = runParser removeComments source


-- I guess I've got to have some way of distinguishing between pre-, in-, and post- operators. Either limit to certain operators/characters or by spaces or something.

-- I feel like this might be able to replace most of the expression parser
-- I suppose it can't do things like Call, If, and other things with more than two arguments
-- expressionParser :: Op.OperatorTable Expression -> Parser Expression
-- expressionParser opTable = Op.buildExprParser opTable _?

-- If I don't end up using Op.buildExprParser, then I should make a (ReaderT Op.OperatorTable Parser Expression)

-- type ParserWithOps = ReaderT (Op.OperatorTable Expression) Parser Expression

{-
--newtype ParserWithOps = ParserWithOps ReaderT (Op.OperatorTable Expression) Parser Expression

instance lazyReaderT :: Lazy ParserWithOps where
    defer f = ReaderT (defer (runReaderT <<< f))
    -}

{-
-- Maybe should use more "try"s
expressionParser :: Op.OperatorTable Expression -> Parser Expression
expressionParser opTable = fix $ \self -> do
    skipSpaces
    left <- prefixParser opTable self
    _ <- ((\_ -> String "") <$> skipSpaces)
    maybeExpr <- optionMaybe (infixParser opTable self left)
    pure $ fromMaybe left maybeExpr
    
    -- Can I do operator parsing manually by putting infix and postfix stuff here?
    -- Need to also consider function calls. Might be tricky to deal with as well. Might be simpler if roll into one solution.
        -- If an identifier or paren group are next to something, it is considered called with that thing. Unless it's being passed into something? I guess it just depends.
    where
        toExp = (\ws -> String ws) <$> whiteSpace

prefixParser :: Op.OperatorTable Expression -> Parser Expression -> Parser Expression
prefixParser opTable expParser = (numberExpr)
    <|> (stringExpr)
    <|> parenExpr expParser
    <|> ifParser expParser
    <|> assignmentExpr expParser
    <|> prefix opTable expParser
    <|> (identExpr)
    -}

factor :: Parser Expression -> Parser Expression
factor expParser = numberExpr
    <|> (stringExpr)
    <|> parenExpr expParser
    <|> ifParser expParser
    <|> assignmentExpr expParser
    <|> (identExpr)
    
expressionParser :: Op.OperatorTable Expression -> Parser Expression
expressionParser opTable = fix $ \self -> do
    skipSpaces
    exp <- Op.buildExprParser opTable (factor self)
    skipSpaces
    pure exp

infixParser :: Op.OperatorTable Expression -> Parser Expression -> Expression -> Parser Expression
infixParser ops expParser left = (infixOpParser ops expParser left)
                        -- <|> (postFix opTable)

createInfix :: String -> Op.Operator Expression
createInfix name = Op.Infix op Op.AssocNone
    where
      op = do
             parsedName <- string name
             pure (\l r -> Infix l parsedName r)

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
reserved :: Array Name
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
parenExpr :: Parser Expression -> Parser Expression
parenExpr expParser = do
    _ <- char '('
    expr <- expParser
    _ <- char ')'
    pure $ Prefix "(" expr

-- Need to limit to non-reserved things.
assignmentExpr :: Parser Expression -> Parser Expression
assignmentExpr expParser = do
    _ <- try do
       _ <- strSkip $ string "let"
       spaces <- whiteSpace
       if spaces == "" then fail "Not let" else pure ""
    name <- nameParser
    skipSpaces
    _ <- string "="
    skipSpaces
    assignedVal <- expParser
    skipSpaces
    _ <- do
       _ <- strSkip $ string "in"
       spaces <- whiteSpace
       if spaces == "" then fail "Not in" else pure ""
    body <- expParser
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

-- TODO Should use the operator table
-- Also, can probably simplify with operator table.
prefix :: Op.OperatorTable Expression -> Parser Expression -> Parser Expression
prefix opTable expParser = do
    let prefixPred op = case op of
                            Op.Prefix x -> Just x
                            _ -> Nothing
    let maybePrefixes = (map prefixPred) <$> opTable
    let prefixes = catMaybes <$> maybePrefixes

    skipSpaces
    createOp <- choice (choice <$> prefixes)
    skipSpaces -- ?

    expr <- expParser
    pure $ createOp expr

infixOpParser :: Op.OperatorTable Expression -> Parser Expression -> Expression -> Parser Expression
infixOpParser opTable expParser left = do
    let toInfix op = case op of
                        Op.Infix inOp _ -> Just inOp
                        _ -> Nothing
    let infixOpTable = (map toInfix) <$> opTable
    let folder new accum = maybe accum (\x -> x: accum) new
    let filteredOps = (foldr folder []) <$> infixOpTable
    -- Need to figure out how to actually use it unflattened
    let ops = foldl (<>) [] filteredOps

    skipSpaces
    createOp <- choice ops
    skipSpaces
    right <- expParser
    pure (createOp left right)

ifParser :: Parser Expression -> Parser Expression
ifParser expParser = do
    _ <- try do
       _ <- strSkip $ string "if"
       spaces <- whiteSpace
       if spaces == "" then fail "Not if" else pure ""
    pred <- expParser
    skipSpaces
    _ <- do
       _ <- strSkip $ string "then"
       spaces <- whiteSpace
       if spaces == "" then fail "Not then" else pure ""
    thn <- expParser
    skipSpaces
    _ <- do
       _ <- strSkip $ string "else"
       spaces <- whiteSpace
       if spaces == "" then fail "Not else" else pure ""
    els <- expParser
    pure $ If pred thn els

infixOp :: Op.Assoc -> String -> Op.Operator Expression
infixOp assoc opStr = Op.Infix parser assoc
    where
      parser = try $ do
         skipSpaces
         _ <- string opStr
         skipSpaces
         pure (\left -> \right -> Infix left opStr right)

prefixOp :: String -> Op.Operator Expression
prefixOp opStr = Op.Prefix do
    _ <- string opStr
    pure (\right -> Prefix opStr right)

postfixOp :: String -> Op.Operator Expression
postfixOp opStr = Op.Postfix do
    _ <- string opStr
    pure (\left -> Postfix left opStr)
