module Simmer.Interface where

import Prelude
import Effect (Effect)
import Data.Either (Either(..), note)
import Data.Int (toNumber, fromNumber)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Array (uncons)
import Data.Traversable (sequence)

import Simmer.Symbol (Symbol)
import Simmer.Interpret (Value(..), Tag, TagSet(..), SimmerFn)

{-
    Functionality to have the host language (Purescript) interface with the embedded language (Simmer).
    Could maybe rename to "Convert" or something less generic.

    Now that I think about it, how is this supposed to work with the currying I want to implement in the future?
    -}

class ToValue a where
    toValue :: a -> Value

instance toValueString :: ToValue String where
    toValue = StringVal

instance toValueNumber :: ToValue Number where
    toValue = NumberVal

instance toValueInt :: ToValue Int where
    toValue = toValue <<< toNumber

instance toValueTag :: ToValue Tag where
    toValue = TagVal

instance toValueFn :: ToValue SimmerFn where
    toValue = FunctionVal

instance toValueArray :: ToValue (Array Value) where
    toValue = ListVal

instance toValueValue :: ToValue Value where
    toValue = identity

instance toValueTagSet :: ToValue TagSet where
    toValue = TagSetVal

instance toValueTagMap :: ToValue (Map Symbol Value) where
    toValue = TagSetVal <<< TagSet

class FromValue a where
    fromValue :: Value -> Either String a

instance fromValueValue :: FromValue Value where
    fromValue = Right

instance fromValueString :: FromValue String where
    fromValue (StringVal s) = Right s
    fromValue _ = Left "Expected String"

instance fromValueNumber :: FromValue Number where
    fromValue (NumberVal n) = Right n
    fromValue _ = Left "Expected Number"

instance fromValueInt :: FromValue Int where
    fromValue (NumberVal n) = case fromNumber n of
                                  Just i -> Right i
                                  Nothing -> Left "Got Number but expected Integer"
    fromValue _ = Left "Expected Number that is an Integer"

instance fromValueTag :: FromValue Tag where
    fromValue (TagVal t) = Right t
    fromValue _ = Left "Expected Tag"
 
instance fromValueList :: (FromValue a) => FromValue (Array a) where
    fromValue (ListVal l) = sequence $ map fromValue l
    fromValue _ = Left "Expected List"

instance fromValueTagSet :: FromValue TagSet where
    fromValue (TagSetVal ts) = Right ts
    fromValue _ = Left "Expected TagSet"

instance fromValueTagMap :: FromValue (Map Symbol Value) where
    fromValue (TagSetVal (TagSet tm)) = Right tm
    fromValue _ = Left "Expected TagSet"

{- TODO fromValueFunctionVal
    Not sure how to do fromValueFn
    I would like to convert directly to purescript function, but the types probably make that difficult
    Perhaps have the type be `Array Value -> Effect (Either String Value)` and convert `Fn` to that type. Eval lambda.
    -}

-- Shout out to the following StackOverflow Q/A that helped create this class: https://stackoverflow.com/questions/18154615/lowering-functions-to-an-embedded-language
class ConvertHostFn r where
    convertFn :: r -> Array Value -> Effect (Either String Value)

instance convertHostFnNumber :: ConvertHostFn Number where
    convertFn n [] = pure <<< Right <<< toValue $ n
    convertFn n nonEmpty = pure <<< Left $ "Given more arguments than expected"

instance convertHostFnNumberFn :: (ConvertHostFn r) => ConvertHostFn (Number -> r) where
    convertFn fn [] = pure $ Left "Given fewer arguments than expected"
    convertFn fn arr = do
       let eParams = do
                        { head: x, tail: y } <- note "Given fewer arguments than expected" $ uncons arr
                        val <- fromValue x
                        pure { val, tail: y }
       case eParams of
           Left err -> pure <<< Left $ err
           Right { val: x, tail: y } -> convertFn (fn x) y

genericConvertResult :: forall r. (ToValue r) => Effect (Either String r) -> Array Value -> Effect (Either String Value)
genericConvertResult x [] = (map toValue) <$> x
genericConvertResult x nonEmpty = do
    _ <- x
    pure <<< Left $ "Given more arguments than expected"

-- genericConvertFull :: forall a b. (FromValue a) => (ConvertHostFn b) => 

genericValueConvertFn :: forall r. (ToValue r) => r -> Array Value -> Effect (Either String Value)
genericValueConvertFn x [] = pure <<< Right <<< toValue $ x
genericValueConvertFn x nonEmpty = pure <<< Left $ "Given more arguments than expected"

genericFnConvertFn :: forall a b. (FromValue a) => (ConvertHostFn b) => (a -> b) -> Array Value -> Effect (Either String Value)
genericFnConvertFn fn [] = pure $ Left "Given fewer arguments than expected"
genericFnConvertFn fn arr = do
   let eParams = do
                    { head: x, tail: y } <- note "Given fewer arguments than expected" $ uncons arr
                    val <- fromValue x
                    pure { val, tail: y }
   case eParams of
       Left err -> pure <<< Left $ err
       Right { val: x, tail: y } -> convertFn (fn x) y

instance convertHostFnResult :: (ToValue r) => ConvertHostFn (Effect (Either String r)) where
    convertFn = genericConvertResult

instance convertHostFnString :: ConvertHostFn String where
    convertFn = genericValueConvertFn

instance convertHostFnStringFn :: (ConvertHostFn r) => ConvertHostFn (String -> r) where
    convertFn = genericFnConvertFn

instance convertHostFnInt :: ConvertHostFn Int where
    convertFn = genericValueConvertFn

instance convertHostFnIntFn :: (ConvertHostFn r) => ConvertHostFn (Int -> r) where
    convertFn = genericFnConvertFn

instance convertHostFnTag :: ConvertHostFn Tag where
    convertFn = genericValueConvertFn

instance convertHostFnTagFn :: (ConvertHostFn r) => ConvertHostFn (Tag -> r) where
    convertFn = genericFnConvertFn

instance convertHostFnValue :: ConvertHostFn Value where
    convertFn = genericValueConvertFn

instance convertHostFnValueFn :: (ConvertHostFn r) => ConvertHostFn (Value -> r) where
    convertFn = genericFnConvertFn
