{-@ LIQUID "--reflection" @-}
{-@ LIQUID "--ple" @-}
module Semigroup where

import Prelude hiding (Semigroup, mappend)


infixl 3 ==.

(==.) :: a -> a -> a
_ ==. x = x
{-# INLINE (==.) #-}

data QED = QED

infixl 2 ***
x *** QED = ()


{-@ class YYSemigroup a where
    mappend :: a -> a -> a
    lawAssociative :: v:a -> v':a -> v'':a -> {mappend (mappend v v') v'' == mappend v (mappend v' v'')}
@-}

class YYSemigroup a where
    mappend :: a -> a -> a
    lawAssociative :: a -> a -> a -> ()


{-@ reflect $cmappend @-}

-- sadly, this does not work
-- the error message reveals very useful information
newtype Op a = Op a

{-@ reflect mappendOp @-}
mappendOp :: YYSemigroup a => Op a -> Op a -> Op a
mappendOp (Op a) (Op a') = Op (mappend a' a)

{-@ reflect lawAssociativeOp @-}
lawAssociativeOp :: YYSemigroup a => Op a -> Op a -> Op a -> ()
lawAssociativeOp (Op a) (Op a') (Op a'') = lawAssociative a a' a''


instance (YYSemigroup a) => YYSemigroup (Op a) where
  mappend m n  = mappendOp m n
  lawAssociative m n p =  lawAssociativeOp m n p
