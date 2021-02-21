module Parse where

import Prelude
import Control.Alt ((<|>))
import Text.Parsing.StringParser (Parser, fail)
import Text.Parsing.StringParser.CodePoints (string, anyDigit)
import Text.Parsing.StringParser.Combinators (many1)
import Data.Number (fromString)
import Data.String.CodeUnits (singleton)
import Data.Maybe (Maybe(..))
import Data.Traversable (foldMap)

import Ast (Expression(..))

parse :: String -> Expression
parse source = String "Not implemented"

number :: Parser Number
number = do
    neg <- string "-" <|> pure ""
    whole <- many1 anyDigit
    dec <- string "." <|> pure ""
    less <- if dec == "." then foldMap singleton <$> (many1 anyDigit) else pure ""
    let numString = neg <> (foldMap singleton $ whole) <> dec <> less
    case fromString numString  of
        Just num -> pure num
        Nothing -> fail "Couldn't parse number"
