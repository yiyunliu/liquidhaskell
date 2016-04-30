module spec Data.Tuple where

qualif Fst(v:a, y:b): (v = (fst y)) 
qualif Snd(v:a, y:b): (v = (snd y))

measure fst :: (a,b) -> a
fst (a,b) = a

measure snd :: (a,b) -> b
snd (a,b) = b

fst
    :: x:(a,b) -> {v:a | v = (fst x)}

snd
    :: x:(a,b) -> {v:b | v = (snd x)}
