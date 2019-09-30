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

-- {-@ data YYSemigroupM = YYSemigroupM
--     { mappendD :: a -> a -> a
--     , lawAssociativeD :: v:a -> v':a -> v'':a ->
--         {mappendD v (mappendD v' v'') == mappendD (mappendD v v') v''}
--     } @-}
-- data YYSemigroupM a = YYSemigroupM {mappendD :: a -> a -> a, lawAssociativeD :: a -> a -> a -> ()}
  
{-@ data ThingBA a = ThingBA {mult :: a -> a -> a, assoc :: v:a -> v':a -> v'':a -> {mult (mult v v') v'' == mult v (mult v' v'')}} @-}
data ThingBA a = ThingBA {mult :: a -> a -> a, assoc :: a -> a -> a -> ()}


{-@ class YYSemigroup a where
    mappend :: a -> a -> a
    lawAssociative :: v:a -> v':a -> v'':a -> {mappend (mappend v v') v'' == mappend v (mappend v' v'')}
@-}


-- What happens if we don't lift it...
-- makeHaskellDataDecls would still make the data, but if we dump the measures we won't see mappendG or lawAssociativeG
data YYSemigroupG a = YYSemigroupG {mappendG :: a -> a -> a, lawAssociativeG :: a -> a -> a -> ()}

class YYSemigroup a where
--  {-@ reflect mappend @-}
    mappend :: a -> a -> a
    lawAssociative :: a -> a -> a -> ()

{-@ reflect mappendInt @-}
mappendInt :: Int -> Int -> Int
mappendInt a b = a + b

f :: Int -> Int -> Int
f a b = a + b

-- {-@ testTheorem :: v:Int -> v':Int -> {v + v' = v' - v} @-}
-- testTheorem :: Int -> Int -> ()
-- testTheorem v v' = const () (f v v')


{-@ reflect $cmappend @-}
-- {-@ assume fff :: { mappendInt = $cmappend  } @-}
-- fff :: ()
-- fff = ()

-- {-@ mappendAssociative :: v:Int -> v':Int -> v'':Int -> {mappendInt (mappendInt v v') v''  == mappendInt v (mappendInt v' v'') } @-}
-- mappendAssociative :: Int -> Int -> Int -> ()
-- mappendAssociative x y z =
--         mappendInt (mappendInt x y) z 
--     ==. (x + y) + z
--     ==. x + (y + z)
--     ==. mappendInt x (mappendInt y z)
--     *** QED

{-@  reflect carrot  @-}
carrot :: Int
carrot = 2

-- instance YYSemigroup Int where
--     -- mappend a b = a ^ b
--     mappend a b = a * b

--     lawAssociative x y z = ()--mappendAssociative x y z

data MyNat = Z | S MyNat

-- {-@ reflect natAdd @-}
-- natAdd :: MyNat -> MyNat -> MyNat
-- natAdd Z n' = n'
-- natAdd (S n) n' = S (natAdd n n')


-- {-@ natLawAssociative :: m:MyNat -> n:MyNat -> p:MyNat -> {natAdd (natAdd m n) p = natAdd m (natAdd n p) }@-}
-- natLawAssociative :: MyNat -> MyNat -> MyNat -> ()
-- natLawAssociative Z n p = natAdd (natAdd Z n) p
--                      ==. natAdd n p
--                      ==. natAdd Z (natAdd n p)
--                      *** QED
-- natLawAssociative (S m) n p = natAdd (natAdd (S m) n) p
--                          ==. natAdd (S (natAdd m n)) p
--                          ==. S (natAdd (natAdd m n) p)
--                          ==. const (S (natAdd m (natAdd n p) )) (natLawAssociative m n p)
--                          ==. natAdd (S m) (natAdd n p)
--                          *** QED


-- TRY THIS : SAFE
-- instance YYSemigroup MyNat where
--   mappend m n  = natAdd m n
--   lawAssociative m n p =  natLawAssociative m n p


  

-- TRY THIS : Should be SAFE, but throws unification error

-- instance YYSemigroup MyNat where
--   mappend Z n' = n'
--   mappend (S n) n' = S (mappend n n') 

--   lawAssociative Z n p = mappend (mappend Z n) p
--                      ==. mappend n p
--                      ==. mappend Z (mappend n p)
--                      *** QED
--   lawAssociative (S m) n p = mappend (mappend (S m) n) p
--                          ==. mappend (S (mappend m n)) p
--                          ==. S (mappend (mappend m n) p)
--                          ==. const (S (mappend m (mappend n p) )) (lawAssociative m n p)
--                          ==. mappend (S m) (mappend n p)
--                          *** QED
    
    

-- {-@ reflect natAdd @-}
-- natAdd :: Nat -> Nat -> Nat
-- natAdd Z n' =  n'
-- natAdd (S n) n' = S (natAdd n n')


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
-- {-@ data ThingMM = ThingQQ {f :: Int -> Int, fprop :: v:Int -> {f v >= 0}} @-}
-- data ThingMM =
--   ThingQQ
--     { f :: Int -> Int
--     , fprop :: Int -> ()
--     }

-- {-@ safe' :: t:ThingMM -> v:Int -> {f t v >= 0} @-}
-- safe' :: ThingMM -> Int -> ()
-- safe' (ThingQQ _ fprop) i = const () $ fprop i


