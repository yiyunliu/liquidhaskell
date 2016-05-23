module Alias02 () where

import Alias01

{-@ type LeNum a N = {v: a | v <= N} @-}

{-@ type NegInt = LeNum Int {0} @-}

{-@ type OneInt = GeNum Int {1} @-}

{-@ myabs :: Int -> PosInt @-}
myabs :: Int -> Int
myabs x = if (x > 0) then x else (0 - x)

{-@ myabs :: Int -> OneInt @-}
myabs' :: Int -> Int
myabs' x = if (x >= 1) then x else (1 - x)

{-@ mynabs :: Int -> NegInt @-}
mynabs :: Int -> Int
mynabs x = if (x < 0) then x else (0 - x)

{-@ decr :: x:Int -> LeNum Int {x} @-}
decr :: Int -> Int
decr x = x - 1

