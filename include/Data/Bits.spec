module spec Data.Bits where

import GHC.Base

// TODO: cannot use this because `Bits` is not a `Num`
// assume shiftR
//     :: (Data.Bits.Bits a)
//     => x:a
//     -> d:Nat 
//     -> {v:a | ((d=1) => (x <= 2*v + 1 && 2*v <= x)) }
