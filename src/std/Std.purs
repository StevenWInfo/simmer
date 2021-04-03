module Std where

import Interpret (Library)
import Std.Random as Random
{-
    Currently just a convenience module to get all of the stajndard libraries bundled together.
    -}

libraries :: Array Library
libraries =
    [ Random.library
    ]
