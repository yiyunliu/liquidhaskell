module spec Data.ByteString.Char8 where

import Data.ByteString

import GHC.Base
import GHC.Num

empty
    :: { bs : Data.ByteString.ByteString | bslen bs == 0 }

singleton
    :: GHC.Types.Char
    -> { bs : Data.ByteString.ByteString | bslen bs == 1 }

pack
    :: w8s : [GHC.Types.Char]
    -> { bs : Data.ByteString.ByteString | bslen bs == len w8s }

unpack
    :: bs : Data.ByteString.ByteString
    -> { w8s : [GHC.Types.Char] | len w8s == bslen bs }

cons
    :: GHC.Types.Char
    -> i : Data.ByteString.ByteString
    -> { o : Data.ByteString.ByteString | bslen o == bslen i + 1 }

snoc
    :: i : Data.ByteString.ByteString
    -> GHC.Types.Char
    -> { o : Data.ByteString.ByteString | bslen o == bslen i + 1 }

append
    :: l : Data.ByteString.ByteString
    -> r : Data.ByteString.ByteString
    -> { o : Data.ByteString.ByteString | bslen o == bslen l + bslen r }

head
    :: { bs : Data.ByteString.ByteString | 1 <= bslen bs } -> GHC.Types.Char

uncons
    :: i : Data.ByteString.ByteString
    -> Data.Maybe.Maybe (GHC.Types.Char, { o : Data.ByteString.ByteString | bslen o == bslen i - 1 })

unsnoc
    :: i : Data.ByteString.ByteString
    -> Data.Maybe.Maybe ({ o : Data.ByteString.ByteString | bslen o == bslen i - 1 }, GHC.Types.Char)

last
    :: { bs : Data.ByteString.ByteString | 1 <= bslen bs } -> GHC.Types.Char

tail
    :: { bs : Data.ByteString.ByteString | 1 <= bslen bs } -> GHC.Types.Char

init
    :: { bs : Data.ByteString.ByteString | 1 <= bslen bs } -> GHC.Types.Char

null
    :: bs : Data.ByteString.ByteString
    -> { b : GHC.Types.Bool | Prop b <=> bslen bs == 0 }

length
    :: bs : Data.ByteString.ByteString -> { n : Nat | bslen bs == n }

map
    :: (GHC.Types.Char -> GHC.Types.Char)
    -> i : Data.ByteString.ByteString
    -> { o : Data.ByteString.ByteString | bslen o == bslen i }

reverse
    :: i : Data.ByteString.ByteString
    -> { o : Data.ByteString.ByteString | bslen o == bslen i }

intersperse
    :: GHC.Types.Char
    -> i : Data.ByteString.ByteString
    -> { o : Data.ByteString.ByteString | (bslen i == 0 <=> bslen o == 0) && (1 <= bslen i <=> bslen o == 2 * bslen i - 1) }

intercalate
    :: l : Data.ByteString.ByteString
    -> rs : [Data.ByteString.ByteString]
    -> { o : Data.ByteString.ByteString | len rs == 0 ==> bslen o == 0 }

transpose
    :: is : [Data.ByteString.ByteString]
    -> { os : [{ bs : Data.ByteString.ByteString | bslen bs <= len is }] | len is == 0 ==> len os == 0}

foldl1
    :: (GHC.Types.Char -> GHC.Types.Char -> GHC.Types.Char)
    -> { bs : Data.ByteString.ByteString | 1 <= bslen bs }
    -> GHC.Types.Char

foldl1'
    :: (GHC.Types.Char -> GHC.Types.Char -> GHC.Types.Char)
    -> { bs : Data.ByteString.ByteString | 1 <= bslen bs }
    -> GHC.Types.Char

foldr1
    :: (GHC.Types.Char -> GHC.Types.Char -> GHC.Types.Char)
    -> { bs : Data.ByteString.ByteString | 1 <= bslen bs }
    -> GHC.Types.Char

foldr1'
    :: (GHC.Types.Char -> GHC.Types.Char -> GHC.Types.Char)
    -> { bs : Data.ByteString.ByteString | 1 <= bslen bs }
    -> GHC.Types.Char

concat
    :: is : [Data.ByteString.ByteString]
    -> { o : Data.ByteString.ByteString | len is == 0 ==> bslen o }

concatMap
    :: (GHC.Types.Char -> Data.ByteString.ByteString)
    -> i : Data.ByteString.ByteString
    -> { o : Data.ByteString.ByteString | bslen i == 0 ==> bslen o == 0 }

any
    :: (GHC.Types.Char -> GHC.Types.Bool)
    -> bs : Data.ByteString.ByteString
    -> { b : GHC.Types.Bool | bslen bs == 0 ==> not (Prop b) }

all
    :: (GHC.Types.Char -> GHC.Types.Bool)
    -> bs : Data.ByteString.ByteString
    -> { b : GHC.Types.Bool | bslen bs == 0 ==> Prop b }

maximum
    :: { bs : Data.ByteString.ByteString | 1 <= bslen bs } -> GHC.Types.Char

minimum
    :: { bs : Data.ByteString.ByteString | 1 <= bslen bs } -> GHC.Types.Char

scanl
    :: (GHC.Types.Char -> GHC.Types.Char -> GHC.Types.Char)
    -> GHC.Types.Char
    -> i : Data.ByteString.ByteString
    -> { o : Data.ByteString.ByteString | bslen o == bslen i }

scanl1
    :: (GHC.Types.Char -> GHC.Types.Char -> GHC.Types.Char)
    -> i : { i : Data.ByteString.ByteString | 1 <= bslen i }
    -> { o : Data.ByteString.ByteString | bslen o == bslen i }

scanr
    :: (GHC.Types.Char -> GHC.Types.Char -> GHC.Types.Char)
    -> GHC.Types.Char
    -> i : Data.ByteString.ByteString
    -> { o : Data.ByteString.ByteString | bslen o == bslen i }

scanr1
    :: (GHC.Types.Char -> GHC.Types.Char -> GHC.Types.Char)
    -> i : { i : Data.ByteString.ByteString | 1 <= bslen i }
    -> { o : Data.ByteString.ByteString | bslen o == bslen i }

mapAccumL
    :: (acc -> GHC.Types.Char -> (acc, GHC.Types.Char))
    -> acc
    -> i : Data.ByteString.ByteString
    -> (acc, { o : Data.ByteString.ByteString | bslen o == bslen i })

mapAccumR
    :: (acc -> GHC.Types.Char -> (acc, GHC.Types.Char))
    -> acc
    -> i : Data.ByteString.ByteString
    -> (acc, { o : Data.ByteString.ByteString | bslen o == bslen i })

replicate
    :: n : GHC.Types.Int
    -> GHC.Types.Char
    -> { bs : Data.ByteString.ByteString | bslen bs == n }

unfoldrN
    :: n : GHC.Types.Int
    -> (a -> Data.Maybe.Maybe (GHC.Types.Char, a))
    -> a
    -> ({ bs : Data.ByteString.ByteString | bslen bs <= n }, Data.Maybe.Maybe a)

take
    :: n : GHC.Types.Int
    -> i : Data.ByteString.ByteString
    -> { o : Data.ByteString.ByteString | (n <= 0 <=> bslen o == 0) &&
                                          ((0 <= n && n <= bslen i) <=> bslen o == n) &&
                                          (bslen i <= n <=> bslen o = bslen i) }

drop
    :: n : GHC.Types.Int
    -> i : Data.ByteString.ByteString
    -> { o : Data.ByteString.ByteString | (n <= 0 <=> bslen o == bslen i) &&
                                          ((0 <= n && n <= bslen i) <=> bslen o == bslen i - n) &&
                                          (bslen i <= n <=> bslen o == 0) }

splitAt
    :: n : GHC.Types.Int
    -> i : Data.ByteString.ByteString
    -> ( { l : Data.ByteString.ByteString | (n <= 0 <=> bslen l == 0) &&
                                            ((0 <= n && n <= bslen i) <=> bslen l == n) &&
                                            (bslen i <= n <=> bslen l == bslen i) }
       , { r : Data.ByteString.ByteString | (n <= 0 <=> bslen r == bslen i) &&
                                            ((0 <= n && n <= bslen i) <=> bslen r == bslen i - n) &&
                                            (bslen i <= n <=> bslen r == 0) }
       )

takeWhile
    :: (GHC.Types.Char -> GHC.Types.Bool)
    -> i : Data.ByteString.ByteString
    -> { o : Data.ByteString.ByteString | bslen o <= bslen i }

dropWhile
    :: (GHC.Types.Char -> GHC.Types.Bool)
    -> i : Data.ByteString.ByteString
    -> { o : Data.ByteString.ByteString | bslen o <= bslen i }

span
    :: (GHC.Types.Char -> GHC.Types.Bool)
    -> i : Data.ByteString.ByteString
    -> ( { l : Data.ByteString.ByteString | bslen l <= bslen i }
       , { r : Data.ByteString.ByteString | bslen r <= bslen i }
       )

spanEnd
    :: (GHC.Types.Char -> GHC.Types.Bool)
    -> i : Data.ByteString.ByteString
    -> ( { l : Data.ByteString.ByteString | bslen l <= bslen i }
       , { r : Data.ByteString.ByteString | bslen r <= bslen i }
       )

break
    :: (GHC.Types.Char -> GHC.Types.Bool)
    -> i : Data.ByteString.ByteString
    -> ( { l : Data.ByteString.ByteString | bslen l <= bslen i }
       , { r : Data.ByteString.ByteString | bslen r <= bslen i }
       )

breakEnd
    :: (GHC.Types.Char -> GHC.Types.Bool)
    -> i : Data.ByteString.ByteString
    -> ( { l : Data.ByteString.ByteString | bslen l <= bslen i }
       , { r : Data.ByteString.ByteString | bslen r <= bslen i }
       )

group
    :: i : Data.ByteString.ByteString
    -> [{ o : Data.ByteString.ByteString | 1 <= bslen o && bslen o <= bslen i }]

groupBy
    :: (GHC.Types.Char -> GHC.Types.Char -> GHC.Types.Bool)
    -> i : Data.ByteString.ByteString
    -> [{ o : Data.ByteString.ByteString | 1 <= bslen o && bslen o <= bslen i }]

inits
    :: i : Data.ByteString.ByteString
    -> [{ o : Data.ByteString.ByteString | bslen o <= bslen i }]

tails
    :: i : Data.ByteString.ByteString
    -> [{ o : Data.ByteString.ByteString | bslen o <= bslen i }]

split
    :: GHC.Types.Char
    -> i : Data.ByteString.ByteString
    -> [{ o : Data.ByteString.ByteString | bslen o <= bslen i }]

splitWith
    :: (GHC.Types.Char -> GHC.Types.Bool)
    -> i : Data.ByteString.ByteString
    -> [{ o : Data.ByteString.ByteString | bslen o <= bslen i }]

lines
    :: i : Data.ByteString.ByteString
    -> [{ o : Data.ByteString.ByteString | bslen o <= bslen i }]

words
    :: i : Data.ByteString.ByteString
    -> [{ o : Data.ByteString.ByteString | bslen o <= bslen i }]

unlines
    :: is : [Data.ByteString.ByteString]
    -> { o : Data.ByteString.ByteString | (len is == 0 <=> bslen o == 0) && bslen o >= len is }

unwords
    :: is : [Data.ByteString.ByteString]
    -> { o : Data.ByteString.ByteString | (len is == 0 ==> bslen o == 0) && (1 <= len is ==> bslen o >= len is - 1) }

isPrefixOf
    :: l : Data.ByteString.ByteString
    -> r : Data.ByteString.ByteString
    -> { b : GHC.Types.Bool | bslen l >= bslen r ==> not (Prop b) }

isSuffixOf
    :: l : Data.ByteString.ByteString
    -> r : Data.ByteString.ByteString
    -> { b : GHC.Types.Bool | bslen l > bslen r ==> not (Prop b) }

isInfixOf
    :: l : Data.ByteString.ByteString
    -> r : Data.ByteString.ByteString
    -> { b : GHC.Types.Bool | bslen l > bslen r ==> not (Prop b) }

breakSubstring
    :: il : Data.ByteString.ByteString
    -> ir : Data.ByteString.ByteString
    -> ( { ol : Data.ByteString.ByteString | bslen ol <= bslen ir && (bslen il > bslen ir ==> bslen ol == bslen ir)}
       , { or : Data.ByteString.ByteString | bslen or <= bslen ir && (bslen il > bslen ir ==> bslen or == 0) }
       )

elem
    :: GHC.Types.Char
    -> bs : Data.ByteString.ByteString
    -> { b : GHC.Types.Bool | bslen b == 0 ==> not (Prop b) }

notElem
    :: GHC.Types.Char
    -> bs : Data.ByteString.ByteString
    -> { b : GHC.Types.Bool | bslen b == 0 ==> Prop b }

find
    :: (GHC.Types.Char -> GHC.Types.Bool)
    -> bs : Data.ByteString.ByteString
    -> Data.Maybe.Maybe { w8 : GHC.Types.Char | bslen bs /= 0 }

filter
    :: (GHC.Types.Char -> GHC.Types.Bool)
    -> i : Data.ByteString.ByteString
    -> { o : Data.ByteString.ByteString | bslen o <= bslen i }

partition
    :: (GHC.Types.Char -> GHC.Types.Bool)
    -> i : Data.ByteString.ByteString
    -> ( { l : Data.ByteString.ByteString | bslen l <= bslen i }
       , { r : Data.ByteString.ByteString | bslen r <= bslen i }
       )

index
    :: bs : Data.ByteString.ByteString
    -> { n : GHC.Types.Int | 0 <= n && n < bslen bs }
    -> GHC.Types.Char

elemIndex
    :: GHC.Types.Char
    -> bs : Data.ByteString.ByteString
    -> Data.Maybe.Maybe { n : GHC.Types.Int | 0 <= n && n < bslen bs }

elemIndices
    :: GHC.Types.Char
    -> bs : Data.ByteString.ByteString
    -> [{ n : GHC.Types.Int | 0 <= n && n < bslen bs }]

elemIndexEnd
    :: GHC.Types.Char
    -> bs : Data.ByteString.ByteString
    -> Data.Maybe.Maybe { n : GHC.Types.Int | 0 <= n && n < bslen bs }

findIndex
    :: (GHC.Types.Char -> GHC.Types.Bool)
    -> bs : Data.ByteString.ByteString
    -> Data.Maybe.Maybe { n : GHC.Types.Int | 0 <= n && n < bslen bs }

findIndices
    :: (GHC.Types.Char -> GHC.Types.Bool)
    -> bs : Data.ByteString.ByteString
    -> [{ n : GHC.Types.Int | 0 <= n && n < bslen bs }]

count
    :: GHC.Types.Char
    -> bs : Data.ByteString.ByteString
    -> { n : GHC.Types.Int | 0 <= n && n < bslen bs }

zip
    :: l : Data.ByteString.ByteString
    -> r : Data.ByteString.ByteString
    -> { o : [(GHC.Types.Char, GHC.Types.Char)] | len o <= bslen l && len o <= bslen r }

zipWith
    :: (GHC.Types.Char -> GHC.Types.Char -> a)
    -> l : Data.ByteString.ByteString
    -> r : Data.ByteString.ByteString
    -> { o : [a] | len o <= bslen l && len o <= bslen r }

unzip
    :: i : [(GHC.Types.Char, GHC.Types.Char)]
    -> ( { l : Data.ByteString.ByteString | bslen l == len i }
       , { r : Data.ByteString.ByteString | bslen r == len i }
       )

sort
    :: i : Data.ByteString.ByteString
    -> { o : Data.ByteString.ByteString | bslen o == bslen i }

readInt
    :: i : Data.ByteString.ByteString
    -> Data.Maybe.Maybe { p : (GHC.Types.Int, { o : Data.ByteString.ByteString | bslen o < bslen i}) | bslen i /= 0 }

readInteger
    :: i : Data.ByteString.ByteString
    -> Data.Maybe.Maybe { p : (GHC.Integer.Type.Integer, { o : Data.ByteString.ByteString | bslen o < bslen i}) | bslen i /= 0 }

copy
    :: i : Data.ByteString.ByteString
    -> { o : Data.ByteString.ByteString | bslen o == bslen i }

hGet
    :: System.IO.Handle
    -> n : { n : GHC.Types.Int | 0 <= n }
    -> GHC.Types.IO { bs : Data.ByteString.ByteString | bslen bs == n || bslen bs == 0 }

hGetSome
    :: System.IO.Handle
    -> n : { n : GHC.Types.Int | 0 <= n }
    -> GHC.Types.IO { bs : Data.ByteString.ByteString | bslen bs <= n }

hGetNonBlocking
    :: System.IO.Handle
    -> n : { n : GHC.Types.Int | 0 <= n }
    -> GHC.Types.IO { bs : Data.ByteString.ByteString | bslen bs <= n }
