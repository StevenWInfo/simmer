module Parse where

import Prelude
import Control.Alt ((<|>))
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.CodePoints (string)
import Text.Parsing.StringParser.Combinators (many1)
import Data.Foldable (foldl)

import Ast (Expression(..))

parse :: String -> Expression
parse source = String "Not implemented"

digit :: Parser Int
digit = let strToInt str int = string str >>= \_ -> pure int
         in strToInt "0" 0
         <|> strToInt "1" 1
         <|> strToInt "2" 2
         <|> strToInt "3" 3
         <|> strToInt "4" 4
         <|> strToInt "5" 5
         <|> strToInt "6" 6
         <|> strToInt "7" 7
         <|> strToInt "8" 8
         <|> strToInt "9" 9

integer :: Parser Int
integer = do
   start <- string "-" <|> pure ""
   digits <- many1 digit
   -- Probably need to find a more efficient way. JS FFI if nothing else.
   let int = foldl (\accum new -> (10 * accum) + new) 0 digits
   pure (if start == "-" then -1 * int else int)
