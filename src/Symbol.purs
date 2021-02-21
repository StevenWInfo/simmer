module Symbol where

import Prelude (Unit)

data Symbol = Symbol Unit

foreign import symbol :: String -> Symbol

foreign import eqCompare :: Symbol -> Symbol -> Boolean

foreign import getDescription :: Symbol -> String
