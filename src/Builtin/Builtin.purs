module Builtin where

import Prelude
import Effect (Effect)
import Data.Map (Map, fromFoldable)
import Data.Newtype (over)
import Data.Tuple (Tuple(..))
import Data.Array (uncons, index, length)
import Effect.Class.Console as CON
import Data.Either (Either(..), note)
import Text.Parsing.StringParser.Expr as Op

import Interpret as I
import Interface (convertFn)

-- import Debug.Trace (spy)

{-
    Maybe separate into builtin and prelude stuff.
        Want to be able to "exclude" portions of prelude.
    -}

-- Could make polyvariadic, but want to initially just work with one param.
{-
log :: I.TempForeignFn
log params = do
    -- log $ logStr param
    case param of
        Right rParam -> do
            CON.log $ logStr rParam
            pure <<< Right $ (I.TagVal voidTag)
        otherwise -> pure otherwise
    where
      param :: Either String I.Value
      param = do
         split <- popParam params
         tooLargeCheck split.tail
         Right split.head
      logStr (I.StringVal str) = str
      logStr (I.NumberVal num) = show num
      logStr (I.TagVal (I.Empty)) = "EmptyTag: EmptyTag"
      logStr (I.TagVal (I.Tag t)) = show t.symbol <> ": " <> logStr t.value
      logStr othwerwise = show otherwise
      -}

log :: I.TempForeignFn
log = convertFn doLog
    where
      logStr (I.StringVal str) = str
      logStr (I.NumberVal num) = show num
      logStr (I.TagVal (I.Empty)) = "EmptyTag: EmptyTag"
      logStr (I.TagVal (I.Tag t)) = show t.symbol <> ": " <> logStr t.value
      logStr othwerwise = show otherwise
      doLog :: I.Value -> Effect (Either String I.Value)
      doLog val = do
            CON.log $ logStr val
            pure <<< Right <<< I.TagVal $ voidTag

-- Maybe making an Effect Either monad transformer would make this a bit simpler.
-- TODO Should this concatenate?
-- Fields/monoids and whatnot?
builtinAdd :: I.TempForeignFn
builtinAdd params = pure $ do
    split1 <- popParam params
    split2 <- popParam split1.tail
    tooLargeCheck split2.tail
    add split1.head split2.head

    where
      add :: I.Value -> I.Value -> Either String I.Value
      add (I.NumberVal l) (I.NumberVal r) = Right (I.NumberVal (l + r))
      add _ _ = Left "Expecting numbers to add (+)"

builtinSubtract :: I.TempForeignFn
builtinSubtract params = pure $ do
    sizeCheck params 2
    left <- getParam params 0
    right <- getParam params 1
    subtr left right
    where
      subtr (I.NumberVal l) (I.NumberVal r) = Right (I.NumberVal (l - r))
      subtr _ _ = Left "Expecting numbers to subtract (-)"

builtinMultiply :: I.TempForeignFn
builtinMultiply = convertFn mult
    where
      mult :: Number -> Number -> Effect (Either String Number)
      mult a b = pure $ Right (a * b)

-- Have to limit the type to number
negateNum :: Number -> Number
negateNum = negate

-- May not really need these anymore.
sizeCheck :: forall a. Array a -> Int -> Either String Unit
sizeCheck params expected
    | (length params) < expected = Left "Fewer params than expected"
    | (length params) > expected = Left "More params than expected"
    | otherwise = Right unit

getParam :: forall a. Array a -> Int -> Either String a
getParam params pos = note "Fewer parameters than expected" $ index params pos

popParam :: Array I.Value -> Either String { head :: I.Value, tail :: Array I.Value }
popParam params = note "Fewer parameters than expected" (uncons params)
--popParam params = do
    -- if num > (length params) then Left "Fewer params than expected" else if num < (length params) then Left "More params than expected" else 
    --split <- note "Fewer parameters than expected" (uncons params)

tooLargeCheck :: Array I.Value -> Either String Unit
tooLargeCheck params = if params /= [] then Left "Too many parameters" else Right unit

--builtinFns :: Array (Tuple String I.Value)
builtinFns :: Map String I.Value
builtinFns = (I.FunctionVal <<< I.Foreign) <$> (fromFoldable fns)
    where
      fns =
          [ Tuple "log" log 
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
          , I.Operator (I.Foreign builtinMultiply) (I.Infix "*" Op.AssocRight)
          , I.Operator (I.Foreign (convertFn negateNum)) (I.Prefix "-")
          ]
      }

builtinLibrary :: I.Library
builtinLibrary = Tuple builtinEnv builtinOps
