module Symbol where

import Prelude (Unit, class Show, (<>), class Eq)

data Symbol = Symbol Unit

instance showSymbol :: Show Symbol where
    show s = "Symbol(" <> getDescription s <> ")"

instance eqSymbol :: Eq Symbol where
    eq a b = eqCompare a b

-- Should this have Effect or something? It's not exactly pure. Or is it?
foreign import symbol :: String -> Symbol

foreign import eqCompare :: Symbol -> Symbol -> Boolean

foreign import getDescription :: Symbol -> String
