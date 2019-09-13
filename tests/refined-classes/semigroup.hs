{-@ LIQUID "--reflection" @-}
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
    lawAssociative :: v:a -> v':a -> v'':a -> {mappend v v' == mappend v' v''}
@-}

class YYSemigroup a where
--  {-@ reflect mappend @-}

    mappend :: a -> a -> a

    lawAssociative :: a -> a -> a -> ()

{-@ reflect mappendInt @-}
mappendInt :: Int -> Int -> Int
mappendInt a b = a + b

{-@ mappendAssociative :: v:Int -> v':Int -> v'':Int -> {mappendInt (mappendInt v v') v''  == mappendInt v (mappendInt v' v'') } @-}
mappendAssociative :: Int -> Int -> Int -> ()
mappendAssociative x y z =
        mappendInt (mappendInt x y) z 
    ==. (x + y) + z
    ==. x + (y + z)
    ==. mappendInt x (mappendInt y z)
    *** QED

-- instance YYSemigroup Int where
--     -- mappend a b = a ^ b
--     mappend a b = mappendInt a b

--     lawAssociative x y z = mappendAssociative x y z


-- -- instance YYSemigroup Float where
-- --     -- mappend a b = a ^ b
-- --     mappend a b = a + b

-- --     lawAssociative x y z = 
-- --             mappend (mappend x y) z 
-- --         ==. (x + y) + z
-- --         ==. x + (y + z)
-- --         ==. mappend x (mappend y z)
-- --         *** QED

-- mappend4 :: YYSemigroup a => a -> a -> a -> a -> a
-- mappend4 a b c d = a `mappend` b `mappend` c `mappend` d

-- -- instance Semigroup (Maybe a) where
-- --  ...

-- -- test :: Semigroup a => a -> a -> a
-- -- test x y = mappend x y
{-@ data ThingMM = ThingQQ {f :: Int -> Int, fprop :: v:Int -> {f v >= 0}} @-}
data ThingMM =
  ThingQQ
    { f :: Int -> Int
    , fprop :: Int -> ()
    }

{-@ safe' :: t:ThingMM -> v:Int -> {f t v >= 0} @-}
safe' :: ThingMM -> Int -> ()
safe' (ThingQQ _ fprop) i = const () $ fprop i
