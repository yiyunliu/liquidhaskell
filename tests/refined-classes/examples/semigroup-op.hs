{-@ LIQUID "--reflection" @-}
{-@ LIQUID "--ple" @-}
module Semigroup where

import Prelude hiding (Semigroup, mappend)


class YYSemigroup a where
    mappend :: a -> a -> a
    {-@ lawAssociative :: v:a -> v':a -> v'':a -> {mappend (mappend v v') v'' == mappend v (mappend v' v'')} @-}
    lawAssociative :: a -> a -> a -> ()



-- sadly, this does not work
-- the error message reveals very useful information
newtype Op a = Op a

{-@ reflect mappendOp @-}
mappendOp :: YYSemigroup a => Op a -> Op a -> Op a
mappendOp (Op a) (Op a') = Op (mappend a' a)

-- uncomment me to get a different error message
-- {-@ lawAssociativeOp :: d:YYSemigroup a
--   -> x:Op a
--   -> y:Op a
--   -> z:Op a
--   -> {mappendOp d x (mappendOp d y z) == mappendOp d (mappendOp d x y) z} @-}
-- lawAssociativeOp :: YYSemigroup a => Op a -> Op a -> Op a -> ()
-- lawAssociativeOp (Op a) (Op a') (Op a'') = lawAssociative a'' a' a



instance (YYSemigroup a) => YYSemigroup (Op a) where
  mappend m n  = mappendOp m n
  lawAssociative m n p =  lawAssociativeOp m n p
