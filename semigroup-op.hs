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



-- sadly, this does not work
-- the error message reveals very useful information
data Op a = Op a

{-@ reflect mappendOp @-}
mappendOp :: YYSemigroup a => Op a -> Op a -> Op a
mappendOp (Op a) (Op a') = Op (mappend a' a)

-- uncomment me to get a different error message
{-@ lawAssociativeOp :: d:YYSemigroup a
  -> x:Op a
  -> y:Op a
  -> z:Op a
  -> {mappendOp d x (mappendOp d y z) == mappendOp d (mappendOp d x y) z} @-}
lawAssociativeOp :: YYSemigroup a => Op a -> Op a -> Op a -> ()
lawAssociativeOp (Op a) (Op a') (Op a'') = lawAssociative a'' a' a

-- {-@ mymappend :: a -> a -> a @-}
-- mymappend :: YYSemigroup a => a -> a -> a
-- mymappend = mappend



{-@ myAdd :: a -> a -> a @-}
myAdd :: Num a => a -> a -> a
myAdd = (+)

{-@ xx :: x:{Int | x + 1 > 0} @-}
xx :: Int
xx = 0

{-@ reflect myId @-}
myId :: a -> a
myId x = x

{-@ sillyId :: k:{a | myId k == k } -> y:{a | y == k}@-}
sillyId :: a -> a
sillyId x = x

-- instance (YYSemigroup a) => YYSemigroup (Op a) where
--   mappend m n  = mappendOp m n
--   lawAssociative m n p =  lawAssociativeOp m n p
