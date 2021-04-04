module Builtin.Tags where

import Prelude

import Data.Tuple (Tuple(..))
import Interpret as I

voidTag :: I.Tag
voidTag = I.newTag "Void" I.emptyTagValue

tags :: Array (Tuple String I.Value)
tags =
    [ tagEntry I.Empty
    , tagEntry voidTag
    ]
    where
      tagEntry (I.Empty) = Tuple "EmptyTag" I.emptyTagValue
      tagEntry tag@(I.Tag t) = Tuple (show t.symbol) $ I.TagVal tag
