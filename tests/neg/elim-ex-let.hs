
{-# LANGUAGE QuasiQuotes #-}

module ElimExLet (prop) where

import LiquidHaskell

[lq| type MyNat = {v:Int | 0 <= v} |]

[lq| prop :: a -> MyNat |]
prop _ = let x _ = let y = 0 
                   in
                     y - 3
         in 
           x () + 2
