-- Can't prove that 'bar' in the singleton type of measure 'foo' is connected
-- to the Haskell 'bar'

{-@ measure foo @-}

-- | auto generated "singleton" type for `foo`
{- foo :: xs:[(Int,Int)] -> {v:Int | v == foo xs } -}

foo :: [(Int,Int)] -> Int
foo [] = 0
foo (a:as) = bar a + foo as

{-@ measure bar :: (a, b) -> a @-}
bar :: (a, b) -> a  
bar = fst  
