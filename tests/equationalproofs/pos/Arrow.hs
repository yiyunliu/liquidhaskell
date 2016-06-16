module Arrow where

data Arrow a b = Arr {runFun :: a -> b}

{-@ constant runFun : func(2, [Arrow @(0) @(1); @(0); @(1)]) @-}
