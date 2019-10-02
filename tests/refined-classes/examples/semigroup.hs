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


-- I chose MyNat over integer since z3 can go through even without proving associativity
-- we want to know if the refinements are being checked
data MyNat = Z | S MyNat

{-@ reflect natAdd @-}
natAdd :: MyNat -> MyNat -> MyNat
natAdd Z n' = n'
natAdd (S n) n' = S (natAdd n n')

{-@ natLawAssociative :: m:MyNat -> n:MyNat -> p:MyNat -> {natAdd (natAdd m n) p = natAdd m (natAdd n p) }@-}
natLawAssociative :: MyNat -> MyNat -> MyNat -> ()
natLawAssociative Z _ _ = ()
natLawAssociative (S m) n p = natLawAssociative m n p

-- Liquid verifies whether it's true or not
-- comment out the proof and it will complain so we know it's not blindly accepting
instance YYSemigroup MyNat where
  mappend m n  = natAdd m n
  lawAssociative m n p =  () -- natLawAssociative m n p


data MyQnat = QZ | QS MyQnat

{-@ reflect qnatAdd @-}
qnatAdd :: MyQnat -> MyQnat -> MyQnat
qnatAdd QZ n' = n'
qnatAdd (QS n) n' = QS (qnatAdd n n')

{-@ qnatLawAssociative :: m:MyQnat -> n:MyQnat -> p:MyQnat -> {qnatAdd (qnatAdd m n) p = qnatAdd m (qnatAdd n p) }@-}
qnatLawAssociative :: MyQnat -> MyQnat -> MyQnat -> ()
qnatLawAssociative QZ _ _ = ()
qnatLawAssociative (QS m) n p = qnatLawAssociative m n p

-- Liquid verifies whether it's true or not
-- comment out the proof and it will complain so we know it's not blindly accepting
instance YYSemigroup MyQnat where
  mappend m n  = qnatAdd m n
  lawAssociative m n p =  () -- qnatLawAssociative m n p


instance YYSemigroup Int where
  mappend m n  = m + n
  lawAssociative m n p =  () -- qnatLawAssociative m n p
