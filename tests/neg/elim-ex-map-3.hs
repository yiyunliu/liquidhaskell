{-@ LIQUID "--no-termination" @-}

{-# LANGUAGE QuasiQuotes #-}

module ElimExMap (prop) where

import LiquidHaskell

import Prelude hiding (map)

--------------------------------------------------------------------------
[lq| prop :: List MyEven -> List MyEven |]
prop = map ((+ 0) . (+ 1))
--------------------------------------------------------------------------

[lq| type MyEven = {v:Int | v mod 2 == 0 } |]

data List a = Nil | Cons a (List a)

map f Nil         = Nil
map f (Cons x xs) = Cons (f x) (map f xs)
