module Parse where

import Prelude
import Control.Alt ((<|>))
import Text.Parsing.StringParser (Parser, fail)
import Text.Parsing.StringParser.CodePoints (string, anyDigit, noneOf)
import Text.Parsing.StringParser.Combinators (many1, many)
import Data.Number (fromString)
import Data.Maybe (Maybe(..))
import Data.String.Yarn (fromChars)

import Ast (Expression(..))

parse :: String -> Expression
parse source = String "Not implemented"

number :: Parser Number
number = do
    neg <- string "-" <|> pure ""
    whole <- many1 anyDigit
    dec <- string "." <|> pure ""
    less <- if dec == "." then fromChars <$> (many1 anyDigit) else pure ""
    let numString = neg <> fromChars whole <> dec <> less
    case fromString numString  of
        Just num -> pure num
        Nothing -> fail "Couldn't parse number"

stringExpr :: Parser String
stringExpr = do
    _ <- string "\""
    value <- fromChars <$> (many $ noneOf ['"'])
    _ <- string "\""
    pure value
