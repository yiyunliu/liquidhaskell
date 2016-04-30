module spec Foreign.Concurrent where

import GHC.Ptr
import GHC.Types

newForeignPtr
    :: p:(PtrV a)
    -> GHC.Types.IO ()
    -> (GHC.Types.IO (ForeignPtrN a (plen p)))
