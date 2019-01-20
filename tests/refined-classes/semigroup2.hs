{-@ LIQUID "--reflection" @-}
module SemigroupMod where

import Prelude hiding (Semigroup(..), mappend)


infixl 3 ==.

(==.) :: a -> a -> a
_ ==. x = x
{-# INLINE (==.) #-}

data QED = QED

infixl 2 ***
x *** QED = ()

-- {-@ measure mappend :: Semigroup a => a -> a -> a @-}
-- {-@ class measure mappend :: Semigroup a => a -> a -> a @-}
-- {-@ class measure lawAssociative :: Semigroup a => x : a -> y : a -> z : a -> {mappend (mappend x y) z = mappend x (mappend y z)} @-}
-- 

{-@ reflect method mappend @-}

{-@
class Semigroup a where
    mappend :: a -> a -> a

    lawAssociative 
     :: x : a
     -> y : a
     -> z : a
     -> {v:() | mappend (mappend x y) z = mappend x (mappend y z)}
@-}
--
--   TODO: Get this working XXX
--   -> {mappend (mappend x y) z = mappend x (mappend y z)}
--
-- {-@ reflect Semigroup @-}


class Semigroup a where
    -- {- reflect mappend @-}
    -- {- mappend :: a -> a -> a @-}
    mappend :: a -> a -> a

    -- {- lawAssociative 
    --  :: x : a
    --  -> y : a
    --  -> z : a
    --  -> {mappend (mappend x y) z = mappend x (mappend y z)}
    --  @-}
    -- {- lawAssociative 
    --  :: x : a
    --  -> y : a
    --  -> z : {a | false}
    --  -> ()
    --  @-}
    lawAssociative :: a -> a -> a -> ()

-- instance Semigroup Int where
--     -- mappend a b = a ^ b
--     mappend a b = a + b
-- 
--     lawAssociative x y z = 
--             mappend (mappend x y) z 
--         ==. (x + y) + z
--         ==. x + (y + z)
--         ==. mappend x (mappend y z)
--         *** QED

-- instance Semigroup (Maybe a) where
--  ...

-- test :: Semigroup a => a -> a -> a
-- test x y = mappend x y
