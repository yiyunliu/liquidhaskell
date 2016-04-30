module spec Foreign.Marshal.Array where

import GHC.Ptr
import GHC.Types

allocaArray
    :: Foreign.Storable.Storable a
    => n:GHC.Types.Int
    -> ((PtrN a n) -> GHC.Types.IO b)
    -> GHC.Types.IO b
