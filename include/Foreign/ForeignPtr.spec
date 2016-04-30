module spec Foreign.ForeignPtr where

import GHC.ForeignPtr
import GHC.Types

import Foreign.Concurrent
import Foreign.Ptr

withForeignPtr
    :: fp:(GHC.ForeignPtr.ForeignPtr a)
    -> ((PtrN a (fplen fp))
    -> GHC.Types.IO b)
    -> (GHC.Types.IO b)

newForeignPtr
    :: Foreign.ForeignPtr.FinalizerPtr a
    -> p:(PtrV a)
    -> (GHC.Types.IO (ForeignPtrN a (plen p)))

// this uses `sizeOf (undefined :: a)`, so the ForeignPtr does not necessarily have length `n`
// assume mallocForeignPtrArray
//     :: (Foreign.Storable.Storable a)
//     => n:Nat
//    -> GHC.Types.IO (ForeignPtrN a n)
