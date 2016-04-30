module spec Foreign.Marshal.Alloc where

import GHC.Ptr
import GHC.Types

allocaBytes
    :: n:Nat
    -> (PtrN a n -> GHC.Types.IO b)
    -> GHC.Types.IO b
