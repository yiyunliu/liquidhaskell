module Semigroup where

import Prelude hiding (Semigroup(..), mappend)


infixl 3 ==.

(==.) :: a -> a -> a
_ ==. x = x
{-# INLINE (==.) #-}

data QED = QED

infixl 2 ***
x *** QED = ()

-- YL : rename the class so I can tell it apart from the Semigroup module


data PeanoNat = Z | S PeanoNat deriving (Show, Eq)

{-@ reflect pNatPlus @-}
pNatPlus :: PeanoNat -> PeanoNat -> PeanoNat
pNatPlus Z n' = n'
pNatPlus (S n) n'= S (pNatPlus n n')

data TestData = H Int | J Float

class YYSemigroup a where
--    {-@ reflect mappend @-}
    mappendJJ :: a -> a -> a
    mappendJJ = undefined
    mappendYY :: a -> a -> a


--    {-@ lawAssociative 
--     :: x : a
--     -> y : a
--     -> z : a
--     -> {mappend (mappend x y) z = mappend x (mappend y z)}
--     @-}
    lawAssociative :: a -> a -> a -> ()

    

instance YYSemigroup Int where
    -- mappend a b = a ^ b
    mappendYY a b = a + b
    lawAssociative x y z = 
            mappendYY (mappendYY x y) z 
        ==. (x + y) + z
        ==. x + (y + z)
        ==. mappendYY x (mappendYY y z)
        *** QED

instance YYSemigroup Float where
  mappendYY a b = a + b

  lawAssociative x y z = 
            mappendYY (mappendYY x y) z 
        ==. (x + y) + z
        ==. x + (y + z)
        ==. mappendYY x (mappendYY y z)
        *** QED
-- instance Semigroup (Maybe a) where
--  ...

-- test :: Semigroup a => a -> a -> a
-- test x y = mappend x y


