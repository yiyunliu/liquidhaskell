module spec Data.Foldable where

import GHC.Base

length
    :: Data.Foldable.Foldable f
    => xs:f a
    -> {v:Nat | v = len xs}
