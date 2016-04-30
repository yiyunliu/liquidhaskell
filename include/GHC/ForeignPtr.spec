module spec GHC.ForeignPtr where

import GHC.Types

measure fplen
    :: GHC.ForeignPtr.ForeignPtr a
    -> GHC.Types.Int

type ForeignPtrV a   = {v: (GHC.ForeignPtr.ForeignPtr  a) | 0 <= (fplen v)}
type ForeignPtrN a N = {v: (ForeignPtrV a) | (fplen v) = N }

mallocPlainForeignPtrBytes
    :: n:{v:GHC.Types.Int  | v >= 0 }
    -> (GHC.Types.IO (ForeignPtrN a n))

newForeignPtr_
    :: p:(GHC.Ptr.Ptr a)
    -> (GHC.Types.IO (ForeignPtrN a (plen p)))
