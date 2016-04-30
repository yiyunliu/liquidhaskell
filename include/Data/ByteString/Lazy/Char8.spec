module spec Data.ByteString.Lazy.Char8 where

import Data.ByteString
import Data.ByteString.Lazy

import GHC.Base
import GHC.Int
import GHC.Num

empty
    :: { bs : Data.ByteString.Lazy.ByteString | bllen bs == 0 }

singleton
    :: GHC.Types.Char -> { bs : Data.ByteString.Lazy.ByteString | bllen bs == 1 }

pack
    :: w8s : [GHC.Types.Char]
    -> { bs : Data.ByteString.ByteString | bllen bs == len w8s }

unpack
    :: bs : Data.ByteString.Lazy.ByteString
    -> { w8s : [GHC.Types.Char] | len w8s == bllen bs }

fromStrict
    :: i : Data.ByteString.ByteString
    -> { o : Data.ByteString.Lazy.ByteString | bllen o == bslen i }

toStrict
    :: i : Data.ByteString.Lazy.ByteString
    -> { o : Data.ByteString.ByteString | bslen o == bllen i }

fromChunks
    :: i : [Data.ByteString.ByteString]
    -> { o : Data.ByteString.Lazy.ByteString | len i == 0 <=> bllen o == 0 }

toChunks
    :: i : Data.ByteString.Lazy.ByteString
    -> { os : [{ o : Data.ByteString.ByteString | bslen o <= bllen i}] | len os == 0 <=> bllen i == 0 }

cons
    :: GHC.Types.Char
    -> i : Data.ByteString.Lazy.ByteString
    -> { o : Data.ByteString.Lazy.ByteString | bllen o == bllen i + 1 }

snoc
    :: i : Data.ByteString.Lazy.ByteString
    -> GHC.Types.Char
    -> { o : Data.ByteString.Lazy.ByteString | bllen o == bllen i + 1 }

append
    :: l : Data.ByteString.Lazy.ByteString
    -> r : Data.ByteString.Lazy.ByteString
    -> { o : Data.ByteString.Lazy.ByteString | bllen o == bllen l + bllen r }

head
    :: { bs : Data.ByteString.Lazy.ByteString | 1 <= bllen bs }
    -> GHC.Types.Char

uncons
    :: i : Data.ByteString.Lazy.ByteString
    -> Data.Maybe.Maybe (GHC.Types.Char, { o : Data.ByteString.Lazy.ByteString | bllen o == bllen i - 1 })

unsnoc
    :: i : Data.ByteString.Lazy.ByteString
    -> Data.Maybe.Maybe ({ o : Data.ByteString.Lazy.ByteString | bllen o == bllen i - 1 }, GHC.Types.Char)

last
    :: { bs : Data.ByteString.Lazy.ByteString | 1 <= bllen bs }
    -> GHC.Types.Char

tail
    :: { bs : Data.ByteString.Lazy.ByteString | 1 <= bllen bs }
    -> GHC.Types.Char

init
    :: { bs : Data.ByteString.Lazy.ByteString | 1 <= bllen bs }
    -> GHC.Types.Char

null
    :: bs : Data.ByteString.Lazy.ByteString
    -> { b : GHC.Types.Bool | Prop b <=> bllen bs == 0 }

length
    :: bs : Data.ByteString.Lazy.ByteString -> { n : Nat64 | bllen bs == n }

map
    :: (GHC.Types.Char -> GHC.Types.Char)
    -> i : Data.ByteString.Lazy.ByteString
    -> { o : Data.ByteString.Lazy.ByteString | bllen o == bllen i }

reverse
    :: i : Data.ByteString.Lazy.ByteString
    -> { o : Data.ByteString.Lazy.ByteString | bllen o == bllen i }

intersperse
    :: GHC.Types.Char
    -> i : Data.ByteString.Lazy.ByteString
    -> { o : Data.ByteString.Lazy.ByteString | (bllen i == 0 <=> bllen o == 0) && (1 <= bllen i <=> bllen o == 2 * bllen i - 1) }

intercalate
    :: l : Data.ByteString.Lazy.ByteString
    -> rs : [Data.ByteString.Lazy.ByteString]
    -> { o : Data.ByteString.Lazy.ByteString | len rs == 0 ==> bllen o == 0 }

transpose
    :: is : [Data.ByteString.Lazy.ByteString]
    -> { os : [{ bs : Data.ByteString.Lazy.ByteString | bllen bs <= len is }] | len is == 0 ==> len os == 0}

foldl1
    :: (GHC.Types.Char -> GHC.Types.Char -> GHC.Types.Char)
    -> { bs : Data.ByteString.Lazy.ByteString | 1 <= bllen bs }
    -> GHC.Types.Char

foldl1'
    :: (GHC.Types.Char -> GHC.Types.Char -> GHC.Types.Char)
    -> { bs : Data.ByteString.Lazy.ByteString | 1 <= bllen bs }
    -> GHC.Types.Char

foldr1
    :: (GHC.Types.Char -> GHC.Types.Char -> GHC.Types.Char)
    -> { bs : Data.ByteString.Lazy.ByteString | 1 <= bllen bs }
    -> GHC.Types.Char

foldr1'
    :: (GHC.Types.Char -> GHC.Types.Char -> GHC.Types.Char)
    -> { bs : Data.ByteString.Lazy.ByteString | 1 <= bllen bs }
    -> GHC.Types.Char

concat
    :: is : [Data.ByteString.Lazy.ByteString]
    -> { o : Data.ByteString.Lazy.ByteString | len is == 0 ==> bllen o }

concatMap
    :: (GHC.Types.Char -> Data.ByteString.Lazy.ByteString)
    -> i : Data.ByteString.Lazy.ByteString
    -> { o : Data.ByteString.Lazy.ByteString | bllen i == 0 ==> bllen o == 0 }

any
    :: (GHC.Types.Char -> GHC.Types.Bool)
    -> bs : Data.ByteString.Lazy.ByteString
    -> { b : GHC.Types.Bool | bllen bs == 0 ==> not (Prop b) }

all
    :: (GHC.Types.Char -> GHC.Types.Bool)
    -> bs : Data.ByteString.Lazy.ByteString
    -> { b : GHC.Types.Bool | bllen bs == 0 ==> Prop b }

maximum
    :: { bs : Data.ByteString.Lazy.ByteString | 1 <= bllen bs }
    -> GHC.Types.Char

minimum
    :: { bs : Data.ByteString.Lazy.ByteString | 1 <= bllen bs }
    -> GHC.Types.Char

scanl
    :: (GHC.Types.Char -> GHC.Types.Char -> GHC.Types.Char)
    -> GHC.Types.Char
    -> i : Data.ByteString.Lazy.ByteString
    -> { o : Data.ByteString.Lazy.ByteString | bllen o == bllen i }

scanl1
    :: (GHC.Types.Char -> GHC.Types.Char -> GHC.Types.Char)
    -> i : { i : Data.ByteString.Lazy.ByteString | 1 <= bllen i }
    -> { o : Data.ByteString.Lazy.ByteString | bllen o == bllen i }

scanr
    :: (GHC.Types.Char -> GHC.Types.Char -> GHC.Types.Char)
    -> GHC.Types.Char
    -> i : Data.ByteString.Lazy.ByteString
    -> { o : Data.ByteString.Lazy.ByteString | bllen o == bllen i }

scanr1
    :: (GHC.Types.Char -> GHC.Types.Char -> GHC.Types.Char)
    -> i : { i : Data.ByteString.Lazy.ByteString | 1 <= bllen i }
    -> { o : Data.ByteString.Lazy.ByteString | bllen o == bllen i }

mapAccumL
    :: (acc -> GHC.Types.Char -> (acc, GHC.Types.Char))
    -> acc
    -> i : Data.ByteString.Lazy.ByteString
    -> (acc, { o : Data.ByteString.Lazy.ByteString | bllen o == bllen i })

mapAccumR
    :: (acc -> GHC.Types.Char -> (acc, GHC.Types.Char))
    -> acc
    -> i : Data.ByteString.Lazy.ByteString
    -> (acc, { o : Data.ByteString.Lazy.ByteString | bllen o == bllen i })

replicate
    :: n : Data.Int.Int64
    -> GHC.Types.Char
    -> { bs : Data.ByteString.Lazy.ByteString | bllen bs == n }

unfoldrN
    :: n : GHC.Types.Int
    -> (a -> Data.Maybe.Maybe (GHC.Types.Char, a))
    -> a
    -> ({ bs : Data.ByteString.Lazy.ByteString | bllen bs <= n }, Data.Maybe.Maybe a)

take
    :: n : Data.Int.Int64
    -> i : Data.ByteString.Lazy.ByteString
    -> { o : Data.ByteString.Lazy.ByteString | (n <= 0 ==> bllen o == 0) &&
                                               ((0 <= n && n <= bllen i) <=> bllen o == n) &&
                                               (bllen i <= n <=> bllen o = bllen i) }

drop
    :: n : Data.Int.Int64
    -> i : Data.ByteString.Lazy.ByteString
    -> { o : Data.ByteString.Lazy.ByteString | (n <= 0 <=> bllen o == bllen i) &&
                                               ((0 <= n && n <= bllen i) <=> bllen o == bllen i - n) &&
                                               (bllen i <= n <=> bllen o == 0) }

splitAt
    :: n : Data.Int.Int64
    -> i : Data.ByteString.Lazy.ByteString
    -> ( { l : Data.ByteString.Lazy.ByteString | (n <= 0 <=> bllen l == 0) &&
                                                 ((0 <= n && n <= bllen i) <=> bllen l == n) &&
                                                 (bllen i <= n <=> bllen l == bllen i) }
       , { r : Data.ByteString.Lazy.ByteString | (n <= 0 <=> bllen r == bllen i) &&
                                                 ((0 <= n && n <= bllen i) <=> bllen r == bllen i - n) &&
                                                 (bllen i <= n <=> bllen r == 0) }
       )

takeWhile
    :: (GHC.Types.Char -> GHC.Types.Bool)
    -> i : Data.ByteString.Lazy.ByteString
    -> { o : Data.ByteString.Lazy.ByteString | bllen o <= bllen i }

dropWhile
    :: (GHC.Types.Char -> GHC.Types.Bool)
    -> i : Data.ByteString.Lazy.ByteString
    -> { o : Data.ByteString.Lazy.ByteString | bllen o <= bllen i }

span
    :: (GHC.Types.Char -> GHC.Types.Bool)
    -> i : Data.ByteString.Lazy.ByteString
    -> ( { l : Data.ByteString.Lazy.ByteString | bllen l <= bllen i }
       , { r : Data.ByteString.Lazy.ByteString | bllen r <= bllen i }
       )

spanEnd
    :: (GHC.Types.Char -> GHC.Types.Bool)
    -> i : Data.ByteString.Lazy.ByteString
    -> ( { l : Data.ByteString.Lazy.ByteString | bllen l <= bllen i }
       , { r : Data.ByteString.Lazy.ByteString | bllen r <= bllen i }
       )

break
    :: (GHC.Types.Char -> GHC.Types.Bool)
    -> i : Data.ByteString.Lazy.ByteString
    -> ( { l : Data.ByteString.Lazy.ByteString | bllen l <= bllen i }
       , { r : Data.ByteString.Lazy.ByteString | bllen r <= bllen i }
       )

breakEnd
    :: (GHC.Types.Char -> GHC.Types.Bool)
    -> i : Data.ByteString.Lazy.ByteString
    -> ( { l : Data.ByteString.Lazy.ByteString | bllen l <= bllen i }
       , { r : Data.ByteString.Lazy.ByteString | bllen r <= bllen i }
       )
group
    :: i : Data.ByteString.Lazy.ByteString
    -> [{ o : Data.ByteString.Lazy.ByteString | 1 <= bllen o && bllen o <= bllen i }]

groupBy
    :: (GHC.Types.Char -> GHC.Types.Char -> GHC.Types.Bool)
    -> i : Data.ByteString.Lazy.ByteString
    -> [{ o : Data.ByteString.Lazy.ByteString | 1 <= bllen o && bllen o <= bllen i }]

inits
    :: i : Data.ByteString.Lazy.ByteString
    -> [{ o : Data.ByteString.Lazy.ByteString | bllen o <= bllen i }]

tails
    :: i : Data.ByteString.Lazy.ByteString
    -> [{ o : Data.ByteString.Lazy.ByteString | bllen o <= bllen i }]

split
    :: GHC.Types.Char
    -> i : Data.ByteString.Lazy.ByteString
    -> [{ o : Data.ByteString.Lazy.ByteString | bllen o <= bllen i }]

splitWith
    :: (GHC.Types.Char -> GHC.Types.Bool)
    -> i : Data.ByteString.Lazy.ByteString
    -> [{ o : Data.ByteString.Lazy.ByteString | bllen o <= bllen i }]

lines
    :: i : Data.ByteString.Lazy.ByteString
    -> [{ o : Data.ByteString.Lazy.ByteString | bllen o <= bllen i }]

words
    :: i : Data.ByteString.Lazy.ByteString
    -> [{ o : Data.ByteString.Lazy.ByteString | bllen o <= bllen i }]

unlines
    :: is : [Data.ByteString.Lazy.ByteString]
    -> { o : Data.ByteString.Lazy.ByteString | (len is == 0 <=> bllen o == 0) && bllen o >= len is }

unwords
    :: is : [Data.ByteString.Lazy.ByteString]
    -> { o : Data.ByteString.Lazy.ByteString | (len is == 0 ==> bllen o == 0) && (1 <= len is ==> bllen o >= len is - 1) }

isPrefixOf
    :: l : Data.ByteString.Lazy.ByteString
    -> r : Data.ByteString.Lazy.ByteString
    -> { b : GHC.Types.Bool | bllen l >= bllen r ==> not (Prop b) }

isSuffixOf
    :: l : Data.ByteString.Lazy.ByteString
    -> r : Data.ByteString.Lazy.ByteString
    -> { b : GHC.Types.Bool | bllen l >= bllen r ==> not (Prop b) }

isInfixOf
    :: l : Data.ByteString.Lazy.ByteString
    -> r : Data.ByteString.Lazy.ByteString
    -> { b : GHC.Types.Bool | bllen l >= bllen r ==> not (Prop b) }

breakSubstring
    :: il : Data.ByteString.Lazy.ByteString
    -> ir : Data.ByteString.Lazy.ByteString
    -> ( { ol : Data.ByteString.Lazy.ByteString | bllen ol <= bllen ir && (bllen il > bllen ir ==> bllen ol == bllen ir)}
       , { or : Data.ByteString.Lazy.ByteString | bllen or <= bllen ir && (bllen il > bllen ir ==> bllen or == 0) }
       )

elem
    :: GHC.Types.Char
    -> bs : Data.ByteString.Lazy.ByteString
    -> { b : GHC.Types.Bool | bllen b == 0 ==> not (Prop b) }

notElem
    :: GHC.Types.Char
    -> bs : Data.ByteString.Lazy.ByteString
    -> { b : GHC.Types.Bool | bllen b == 0 ==> Prop b }

find
    :: (GHC.Types.Char -> GHC.Types.Bool)
    -> bs : Data.ByteString.Lazy.ByteString
    -> Data.Maybe.Maybe { w8 : GHC.Types.Char | bllen bs /= 0 }

filter
    :: (GHC.Types.Char -> GHC.Types.Bool)
    -> i : Data.ByteString.Lazy.ByteString
    -> { o : Data.ByteString.Lazy.ByteString | bllen o <= bllen i }

partition
    :: (GHC.Types.Char -> GHC.Types.Bool)
    -> i : Data.ByteString.Lazy.ByteString
    -> ( { l : Data.ByteString.Lazy.ByteString | bllen l <= bllen i }
       , { r : Data.ByteString.Lazy.ByteString | bllen r <= bllen i }
       )

index
    :: bs : Data.ByteString.Lazy.ByteString
    -> { n : Data.Int.Int64 | 0 <= n && n < bllen bs }
    -> GHC.Types.Char

elemIndex
    :: GHC.Types.Char
    -> bs : Data.ByteString.Lazy.ByteString
    -> Data.Maybe.Maybe { n : Data.Int.Int64 | 0 <= n && n < bllen bs }

elemIndices
    :: GHC.Types.Char
    -> bs : Data.ByteString.Lazy.ByteString
    -> [{ n : Data.Int.Int64 | 0 <= n && n < bllen bs }]

elemIndexEnd
    :: GHC.Types.Char
    -> bs : Data.ByteString.Lazy.ByteString
    -> Data.Maybe.Maybe { n : Data.Int.Int64 | 0 <= n && n < bllen bs }

findIndex
    :: (GHC.Types.Char -> GHC.Types.Bool)
    -> bs : Data.ByteString.Lazy.ByteString
    -> Data.Maybe.Maybe { n : Data.Int.Int64 | 0 <= n && n < bllen bs }

findIndices
    :: (GHC.Types.Char -> GHC.Types.Bool)
    -> bs : Data.ByteString.Lazy.ByteString
    -> [{ n : Data.Int.Int64 | 0 <= n && n < bllen bs }]

count
    :: GHC.Types.Char
    -> bs : Data.ByteString.Lazy.ByteString
    -> { n : Data.Int.Int64 | 0 <= n && n < bllen bs }

zip
    :: l : Data.ByteString.Lazy.ByteString
    -> r : Data.ByteString.Lazy.ByteString
    -> { o : [(GHC.Types.Char, GHC.Types.Char)] | len o <= bllen l && len o <= bllen r }

zipWith
    :: (GHC.Types.Char -> GHC.Types.Char -> a)
    -> l : Data.ByteString.Lazy.ByteString
    -> r : Data.ByteString.Lazy.ByteString
    -> { o : [a] | len o <= bllen l && len o <= bllen r }

unzip
    :: i : [(GHC.Types.Char, GHC.Types.Char)]
    -> ( { l : Data.ByteString.Lazy.ByteString | bllen l == len i }
       , { r : Data.ByteString.Lazy.ByteString | bllen r == len i }
       )

sort
    :: i : Data.ByteString.Lazy.ByteString
    -> { o : Data.ByteString.Lazy.ByteString | bllen o == bllen i }

readInt
    :: i : Data.ByteString.Lazy.ByteString
    -> Data.Maybe.Maybe { p : (GHC.Types.Int, { o : Data.ByteString.Lazy.ByteString | bllen o < bllen i}) | bllen i /= 0 }

readInteger
    :: i : Data.ByteString.Lazy.ByteString
    -> Data.Maybe.Maybe { p : (GHC.Integer.Type.Integer, { o : Data.ByteString.Lazy.ByteString | bllen o < bllen i}) | bllen i /= 0 }

copy
    :: i : Data.ByteString.Lazy.ByteString
    -> { o : Data.ByteString.Lazy.ByteString | bllen o == bllen i }

hGet
    :: System.IO.Handle
    -> n : { n : GHC.Types.Int | 0 <= n }
    -> GHC.Types.IO { bs : Data.ByteString.Lazy.ByteString | bllen bs == n || bllen bs == 0 }

hGetNonBlocking
    :: System.IO.Handle
    -> n : { n : Int | 0 <= n }
    -> GHC.Types.IO { bs : Data.ByteString.Lazy.ByteString | bllen bs <= n }
