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
{-@ reflect $cmappend @-}


-- I chose MyNat over integer since z3 can go through even without proving associativity
-- we want to know if the refinements are being checked
data MyNat = Z | S MyNat

{-@ reflect natAdd @-}
natAdd :: MyNat -> MyNat -> MyNat
natAdd Z n' = n'
natAdd (S n) n' = S (natAdd n n')

{-@ reflect natLawAssociative @-}
{-@ natLawAssociative :: m:MyNat -> n:MyNat -> p:MyNat -> {natAdd (natAdd m n) p = natAdd m (natAdd n p) }@-}
natLawAssociative :: MyNat -> MyNat -> MyNat -> ()
natLawAssociative Z n p = natAdd (natAdd Z n) p
                     ==. natAdd n p
                     ==. natAdd Z (natAdd n p)
                     *** QED
natLawAssociative (S m) n p = natAdd (natAdd (S m) n) p
                         ==. natAdd (S (natAdd m n)) p
                         ==. S (natAdd (natAdd m n) p)
                         ==. const (S (natAdd m (natAdd n p) )) (natLawAssociative m n p)
                         ==. natAdd (S m) (natAdd n p)
                         *** QED


-- Liquid verifies whether it's true or not
-- comment out the proof and it will complain so we know it's not blindly accepting
instance YYSemigroup MyNat where
  mappend m n  = natAdd m n
  lawAssociative m n p =  natLawAssociative m n p
