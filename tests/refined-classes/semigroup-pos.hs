{-@ LIQUID "--reflection" @-}
module Semigroup where


{-@ class YYSemigroup a where
    mappend :: a -> a -> a
    lawAssociative :: v:a -> v':a -> v'':a -> ()
@-}

class YYSemigroup a where
    mappend :: a -> a -> a
    lawAssociative :: a -> a -> a -> ()
