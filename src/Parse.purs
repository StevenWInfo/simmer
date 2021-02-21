module Parse where

import Prelude
import Control.Alt ((<|>))
import Text.Parsing.StringParser (Parser, fail, runParser, ParseError, try)
import Text.Parsing.StringParser.CodePoints (string, anyDigit, noneOf, char, eof, whiteSpace, skipSpaces, anyChar, alphaNum)
import Text.Parsing.StringParser.Combinators (many1, many, between, lookAhead, manyTill)
import Data.Number (fromString)
import Data.Maybe (Maybe(..))
import Data.String.Yarn (fromChars)
import Data.Foldable (fold, elem)
import Data.String.CodeUnits (singleton)
import Data.Either (Either)

import Debug.Trace (spy)

import Ast (Expression(..))

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
parse :: String -> Either ParseError Expression
parse source = uncommented >>= runParser expressionParser
    where
      uncommented = runParser removeComments source

expressionParser :: Parser Expression
expressionParser = do
    _ <- (\_ -> String "") <$> skipSpaces
    numberExpr
        <|> stringExpr
        <|> parenExpr
        <|> assignmentExpr
        <|> identExpr
    where
        toExp = (\ws -> String ws) <$> whiteSpace

skip :: forall a. Parser a -> Parser Unit
skip parser = do
    _ <- parser
    pure unit

-- Why did I want this again?
exprSkip :: Parser Expression -> Parser Expression
exprSkip parser = (\_ -> String "") <$> skip parser

strSkip :: Parser String -> Parser Expression
strSkip parser = (\_ -> String "") <$> parser

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
identExpr = (Ident <<< fromChars) <$> (many nameCharacters)

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
parenExpr :: Parser Expression
parenExpr = do
    _ <- char '('
    expr <- expressionParser
    _ <- char ')'
    pure $ Prefix "(" expr

-- Need to limit to non-reserved things.
assignmentExpr :: Parser Expression
assignmentExpr = do
    _ <- try do
       _ <- strSkip $ string "let"
       spaces <- whiteSpace
       if spaces == "" then fail "Not let" else pure ""
    name <- nameParser
    skipSpaces
    _ <- string "="
    assignedVal <- expressionParser
    skipSpaces
    _ <- do
       _ <- strSkip $ string "in"
       spaces <- whiteSpace
       if spaces == "" then fail "Not let" else pure ""
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
    ]
