module spec Foreign.Storable where

import GHC.Ptr
import GHC.Types

import Foreign.Ptr

// DON'T do this, we can't import HS files from SPEC files
// import Language.Haskell.Liquid.Foreign

predicate PValid P N = ((0 <= N) && (N < (plen P)))   

poke
    :: (Foreign.Storable.Storable a)
    => {v: (GHC.Ptr.Ptr a) | 0 < (plen v)}
    -> a
    -> (GHC.Types.IO ())

peek
    :: (Foreign.Storable.Storable a)
    => p:{v: (GHC.Ptr.Ptr a) | 0 < (plen v)}
    -> (GHC.Types.IO {v:a | v = (deref p)})

peekByteOff
    :: (Foreign.Storable.Storable a)
    => forall b. p:(GHC.Ptr.Ptr b)
    -> {v:GHC.Types.Int | (PValid p v)}
    -> (GHC.Types.IO a)

pokeByteOff
    :: (Foreign.Storable.Storable a)
    => forall b. p:(GHC.Ptr.Ptr b)
    -> {v:GHC.Types.Int | (PValid p v)}
    -> a
    -> GHC.Types.IO ()
