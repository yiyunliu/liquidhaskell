module spec GHC.Base where

import GHC.Classes
import GHC.CString
import GHC.IO.Handle
import GHC.Num
import GHC.Prim
import GHC.Types

map
    :: (a -> b)
    -> xs:[a]
    -> {v: [b] | len(v) = len(xs)}

++
    :: xs:[a]
    -> ys:[a]
    -> {v:[a] | (len v) = (len xs) + (len ys)}

$
    :: (a -> b)
    -> a
    -> b

id
    :: x:a
    -> {v:a | v = x}

.
    :: forall <p :: b -> c -> Prop, q :: a -> b -> Prop, r :: a -> c -> Prop>. 
       {xcmp::a, wcmp::b<q xcmp> |- c<p wcmp> <: c<r xcmp>}
       (ycmp:b -> c<p ycmp>)
    -> (zcmp:a -> b<q zcmp>)
    ->  xcmp:a -> c<r xcmp>
