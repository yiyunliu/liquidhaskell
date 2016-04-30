module spec GHC.Num where

import GHC.Prim
import GHC.Types

embed GHC.Integer.Type.Integer as int

fromInteger
    :: (GHC.Num.Num a)
    => x:GHC.Integer.Type.Integer
    -> {v:a | v = x }

negate
    :: (GHC.Num.Num a)
    => x:a
    -> {v:a | v = -x}

+
    :: (GHC.Num.Num a)
    => x:a
    -> y:a
    -> {v:a | v = x + y }

-
    :: (GHC.Num.Num a)
    => x:a
    -> y:a
    -> {v:a | v = x - y }

smallInteger
    :: x:GHC.Prim.Int#
    -> { v:GHC.Integer.Type.Integer | v = (x :: int) }
