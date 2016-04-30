module spec GHC.CString where

import GHC.Prim 
import GHC.Types

unpackCString#
    :: x:GHC.Prim.Addr#
    -> {v:[GHC.Types.Char] | v ~~ x && len v == strLen x}
