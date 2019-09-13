{-@ LIQUID "--reflection" @-}
module Data00Lib where 

{-@ data Thing a = Thing { fldThing :: {v:Int | 0 <= v} , otherThing :: a} @-}
data Thing a = Thing { fldThing :: Int, otherThing :: a }

{-@ reflect test1 @-}
{-@ test1 :: Thing a -> Nat @-}
test1 :: Thing a -> Int
test1 (Thing x _) = x 

{-@ test2 :: Nat -> a -> Thing a @-}
test2 :: Int -> a -> Thing  a
test2 = Thing 


data QED = QED


infixl 3 ==.

(==.) :: a -> a -> a
_ ==. x = x
{-# INLINE (==.) #-}

infixl 2 ***
x *** QED = ()



{-@ obviouslyTrueAndSafe :: t1:Thing a ->  {test1 t1 >= 0}  @-}
obviouslyTrueAndSafe :: Thing a -> ()
obviouslyTrueAndSafe t = test1 t *** QED
  

{-@ obviouslyTrueYetUnsafe' :: t1:Thing a ->  {fldThing t1 >= 0}  @-}
obviouslyTrueYetUnsafe' :: Thing a -> ()
obviouslyTrueYetUnsafe' t1 = fldThing t1 *** QED
