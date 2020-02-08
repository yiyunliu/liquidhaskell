{-@ LIQUID "--reflection" @-}
{-@ LIQUID "--ple" @-}
module Semigroup where

import Prelude hiding (Semigroup, mappend)

{-@ class YYSemigroup a where
    mappend :: a -> a -> a
    lawAssociative :: v:a -> v':a -> v'':a -> {mappend (mappend v v') v'' == mappend v (mappend v' v'')}
@-}

class YYSemigroup a where
    mappend :: a -> a -> a
    lawAssociative :: a -> a -> a -> ()


newtype Op a = Op a


instance (YYSemigroup a) => YYSemigroup (Op a) where
  mappend (Op m) (Op n)  = Op (mappend n m)
  lawAssociative (Op m) (Op n) (Op p) = lawAssociative p n m
