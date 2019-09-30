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

-- I'm really not supposed to do this but it's the fastest way to get the method refined
-- Note that this is not the selector, but the concrete definition of the instances
-- if there were multiple instances, we have to use unique id to disambiguate
{-@ reflect $cmappend @-}


-- I chose MyNat over integer since z3 can go through even without proving associativity
-- we want to know if the refinements are being checked
data MyNat = Z | S MyNat

instance YYSemigroup MyNat where
  mappend Z n' = n'
  mappend (S n) n' = S (mappend n n') 

  lawAssociative Z n p = mappend (mappend Z n) p
                     ==. mappend n p
                     ==. mappend Z (mappend n p)
                     *** QED
  lawAssociative (S m) n p = mappend (mappend (S m) n) p
                         ==. mappend (S (mappend m n)) p
                         ==. S (mappend (mappend m n) p)
                         ==. const (S (mappend m (mappend n p) )) (lawAssociative m n p)
                         ==. mappend (S m) (mappend n p)
                         *** QED
