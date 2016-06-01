module Even where 

{-@ type MyEven = {v:Int | v mod 2 = 0} @-}

{-@ notEven :: Int -> MyEven @-}
notEven :: Int -> Int
notEven x = x * 2
