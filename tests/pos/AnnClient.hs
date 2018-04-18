module AnnClient where 

import Silly 

{-@ test1 :: [{v:Int | v = 1}] @-}
test1 :: [Int]
test1 = [inc 0, dec 2]

-- foo :: V.Vector Int
-- foo = V.fromList [1,2,3]
