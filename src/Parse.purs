module Parse where

import Prelude hiding (between)
import Control.Alt ((<|>))
import Text.Parsing.StringParser (Parser, fail, runParser, ParseError, try)
import Text.Parsing.StringParser.CodePoints (string, anyDigit, noneOf, char, eof, whiteSpace, skipSpaces, alphaNum, oneOf)
import Text.Parsing.StringParser.Combinators (many1, many, between, lookAhead, optionMaybe, endBy1, choice, sepBy)
import Text.Parsing.StringParser.Expr as Op
import Data.Number (fromString)
import Data.Maybe (Maybe(..))
import Data.String.Yarn (fromChars)
import Data.Foldable (fold, elem)
import Data.String.CodeUnits (singleton)
import Data.Either (Either(..))
import Control.Lazy (fix)
import Data.Array ((:), modifyAtIndices)
import Data.List (toUnfoldable)
import Data.List.NonEmpty as NonEmpty

import Ast (Expression(..), Name)

-- import Debug.Trace (spy)

{-
    TODO parse importing

    I'd like to clean up a lot of this code.
    I'd like to use ReaderT to pass around the OperatorTable, but I couldn't figure out how to make a newtype of it an instance of the Lazy typeclass for `fix` to have mutual references.
    Can simplify it a lot with OperatorTable. Might even be able to do most of it with buildExprParser.
    -}

-- TODO Probably should use the actual string parser.
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

emptyCall :: Op.Operator Expression
emptyCall = Op.Postfix do
    _ <- char '!'
    pure EmptyCall

fnApplication :: Op.Operator Expression
fnApplication = Op.Infix parser Op.AssocLeft
    where
      handleNext Nothing = pure (\fn -> \param -> Call fn param)
      handleNext _ = fail "Should backtrack here."
      placeholderEof = (\_ -> "") <$> eof
      parser = try $ do
         spaces <- whiteSpace
         if spaces == "" then fail "Not fn application" else pure unit
         next <- lookAhead <<< optionMaybe <<< choice $ placeholderEof : opParser : (string <$> badStarts)
         handleNext next
      -- I'd like to get actual error here.

-- Just removes comments
parse :: Op.OperatorTable Expression -> String -> Either ParseError Expression
parse opTable source = uncommented >>= runParser (expressionParser modifiedTable)
    where
      uncommented = runParser removeComments source
      addTop top = emptyCall : fnApplication : top
      modifiedTable = modifyAtIndices [0] addTop opTable

-- type ParserWithOps = ReaderT (Op.OperatorTable Expression) Parser Expression

factor :: Parser Expression -> Parser Expression
factor expParser = numberExpr
    <|> stringExpr
    <|> parenExpr expParser
    <|> ifParser expParser
    <|> assignmentExpr expParser
    <|> (try $ listParser expParser)
    <|> (try $ lambdaExpr expParser)
    <|> (try identExpr)
    
expressionParser :: Op.OperatorTable Expression -> Parser Expression
expressionParser opTable = fix $ \self -> do
    skipSpaces
    exp <- Op.buildExprParser opTable (factor self)
    skipSpaces
    pure exp

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

identExprStr :: Parser String
identExprStr = do
    name <- fromChars <$> many1 nameCharacters
    let possibleNum = runParser numberExpr name
    case possibleNum of
        Left _ -> pure unit
        Right _ -> fail "Value is a number, not identifier."
    if elem name reserved then fail msg else pure name
    where
      msg = "Using reserved name in unrecognized way"

identExpr :: Parser Expression
identExpr = Ident <$> identExprStr{-do
    name <- fromChars <$> many1 nameCharacters
    if elem name reserved then fail msg else (pure <<< Ident $ name)
    where
      msg = "Using reserved name in unrecognized way"
      -}

-- Allow alphanumeric (no just numbers) and certain other characters. Probably "'" and "_"
reserved :: Array Name
reserved =
    [ "let"
    , "in"
    --, "\\"
    , "if"
    , "then"
    , "else"
    , "case"
    , "of"
    , "import"
    ] <> badStarts

badStarts :: Array Name
badStarts =
    [ "in"
    , "then"
    , "else"
    , "of"
    , "import"
    , ")"
    , "->"
    ]

nameCharacters :: Parser Char
nameCharacters = alphaNum <|> (char '_') <|> (char '\'')

nameParser :: Parser Name
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
    pure $ Call (Ident "(") expr

-- Need to limit to non-reserved things.
assignmentExpr :: Parser Expression -> Parser Expression
assignmentExpr expParser = do
    _ <- try do
       _ <- strSkip $ string "let"
       spaces <- whiteSpace
       if spaces == "" then fail "Not let" else pure ""
    name <- nameParser
    skipSpaces
    assignedVal <- between eqParser inParser expParser
    body <- expParser
    pure $ Assignment name assignedVal body

    where
      eqParser = do
         _ <- string "="
         skipSpaces
      inParser = do
        _ <- strSkip $ string "in"
        spaces <- whiteSpace
        if spaces == "" then fail "Not in" else pure ""

justName :: Name -> Parser Name
justName name = do
    _ <- strSkip (string name)
    spaces <- whiteSpace
    if spaces == "" then fail ("Not name " <> name) else pure name

lambdaExpr :: Parser Expression -> Parser Expression
lambdaExpr expParser = do
    back <- string "\\"
    params <- identExprStr `endBy1` whiteSpace
    _ <- justName "->"
    exp <- expParser
    pure $ Function (NonEmpty.toUnfoldable params) exp

reservedOperators :: Array String
reservedOperators =
    [ "("
    , ")"
    , "\\"
    , "->"
    , "\""
    , "="
    , "["
    , "]"
    , "!"
    ]

opCharacters :: Array Char
opCharacters =
    [ '('
    , ')'
    , '^'
    , '*'
    , '+'
    , '='
    ]

opParser :: Parser String
opParser = do
    let opCharParser = oneOf opCharacters
    chars <- many1 opCharParser
    pure $ fromChars chars

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
         pure (\left -> \right -> Call (Call (Ident opStr) left) right)

prefixOp :: String -> Op.Operator Expression
prefixOp opStr = Op.Prefix do
    _ <- string opStr
    pure (\right -> Call (Ident opStr) right)

postfixOp :: String -> Op.Operator Expression
postfixOp opStr = Op.Postfix do
    _ <- string opStr
    pure (\left -> Call (Ident opStr) left)

listParser :: Parser Expression -> Parser Expression
listParser expParser = do
    let sepParser = char ',' *> whiteSpace
    let innerParser = sepBy expParser sepParser
    exprs <- between (char '[') (char ']') innerParser
    pure <<< List <<< toUnfoldable $ exprs
