module Builtin where

import Prelude
import Data.Map (Map, fromFoldable)
import Data.Newtype (over)
import Data.Tuple (Tuple(..))
import Data.Array (uncons)
import Effect.Class.Console (log)
import Data.Either (Either(..), note)
import Text.Parsing.StringParser.Expr as Op

import Interpret as I

-- import Debug.Trace (spy)

-- Could make polyvariadic, but want to initially just work with one param.
builtinLog :: I.TempForeignFn
builtinLog params = do
    -- log $ logStr param
    case param of
        Right rParam -> do
            log $ logStr rParam
            pure <<< Right $ (I.TagVal voidTag)
        otherwise -> pure otherwise
    where
      param :: Either String I.Value
      param = do
         split <- getParam params
         tooLargeCheck split.tail
         Right split.head
      logStr (I.StringVal str) = str
      logStr (I.NumberVal num) = show num
      logStr (I.TagVal (I.Empty)) = "EmptyTag: EmptyTag"
      logStr (I.TagVal (I.Tag t)) = show t.symbol <> ": " <> logStr t.value
      logStr othwerwise = show otherwise

-- Maybe making an Effect Either monad transformer would make this a bit simpler.
-- TODO Should this concatenate?
-- Fields/monoids and whatnot?
builtinAdd :: I.TempForeignFn
builtinAdd params = pure $ do
    split1 <- getParam params
    split2 <- getParam split1.tail
    tooLargeCheck split2.tail
    add split1.head split2.head

    where
      add :: I.Value -> I.Value -> Either String I.Value
      add (I.NumberVal l) (I.NumberVal r) = Right (I.NumberVal (l + r))
      add _ _ = Left "Expecting numbers to +"

getParam :: Array I.Value -> Either String { head :: I.Value, tail :: Array I.Value }
getParam params = note "Fewer parameters than expected" (uncons params)
--getParam params = do
    -- if num > (length params) then Left "Fewer params than expected" else if num < (length params) then Left "More params than expected" else 
    --split <- note "Fewer parameters than expected" (uncons params)

tooLargeCheck :: Array I.Value -> Either String Unit
tooLargeCheck params = if params /= [] then Left "Too many parameters" else Right unit

--builtinFns :: Array (Tuple String I.Value)
builtinFns :: Map String I.Value
builtinFns = (I.FunctionVal <<< I.Foreign) <$> (fromFoldable fns)
    where
      fns =
          [ Tuple "log" builtinLog 
          ]

voidTag :: I.Tag
voidTag = I.newTag "Void" I.emptyTagValue

builtinVals :: Array (Tuple String I.Value)
builtinVals =
    [ tagEntry I.Empty
    , tagEntry voidTag
    ]
    where
      tagEntry (I.Empty) = Tuple "EmptyTag" I.emptyTagValue
      tagEntry tag@(I.Tag t) = Tuple (show t.symbol) $ I.TagVal tag

builtinEnv :: I.Environment
builtinEnv = I.Environment
    { values: (fromFoldable builtinVals) <> builtinFns }

builtinOps :: I.Operators
builtinOps = over I.Operators replace I.emptyOperators
    where
      replace empty = empty { first = [ ]
      , sixth =
          [ I.Operator (I.Foreign builtinAdd) (I.Infix "+" Op.AssocRight)
          ]
      }

builtinLibrary :: I.Library
builtinLibrary = Tuple builtinEnv builtinOps
