module Silly where

{-# ANN module "inc :: x:Nat -> {v:Nat | v = x + 1}" #-}
inc :: Int -> Int 
inc x = x + 1

{-# ANN module "dec :: x:Nat -> {v:Nat | v = x - 1}" #-}
dec :: Int -> Int 
dec x = x - 1

{-# ANN module "test :: [{v:Int | v = 1}]" #-}
test :: [Int]
test = [inc 0, dec 2]
