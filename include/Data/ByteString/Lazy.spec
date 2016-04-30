module spec Data.ByteString.Lazy where

import Data.ByteString

import GHC.Base

measure bllen
  :: Data.ByteString.Lazy.ByteString
  -> { n : Data.Int.Int64 | 0 <= n }

invariant { bs : Data.ByteString.Lazy.ByteString | 0 <= bllen bs }

empty
    :: { bs : Data.ByteString.Lazy.ByteString | bllen bs == 0 }

singleton
    :: Data.Word.Word8 -> { bs : Data.ByteString.Lazy.ByteString | bllen bs == 1 }

pack
    :: w8s : [Data.Word.Word8]
    -> { bs : Data.ByteString.ByteString | bllen bs == len w8s }

unpack
    :: bs : Data.ByteString.Lazy.ByteString
    -> { w8s : [Data.Word.Word8] | len w8s == bllen bs }

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
    :: Data.Word.Word8
    -> i : Data.ByteString.Lazy.ByteString
    -> { o : Data.ByteString.Lazy.ByteString | bllen o == bllen i + 1 }

snoc
    :: i : Data.ByteString.Lazy.ByteString
    -> Data.Word.Word8
    -> { o : Data.ByteString.Lazy.ByteString | bllen o == bllen i + 1 }

append
    :: l : Data.ByteString.Lazy.ByteString
    -> r : Data.ByteString.Lazy.ByteString
    -> { o : Data.ByteString.Lazy.ByteString | bllen o == bllen l + bllen r }

head
    :: { bs : Data.ByteString.Lazy.ByteString | 1 <= bllen bs }
    -> Data.Word.Word8

uncons
    :: i : Data.ByteString.Lazy.ByteString
    -> Data.Maybe.Maybe (Data.Word.Word8, { o : Data.ByteString.Lazy.ByteString | bllen o == bllen i - 1 })

unsnoc
    :: i : Data.ByteString.Lazy.ByteString
    -> Data.Maybe.Maybe ({ o : Data.ByteString.Lazy.ByteString | bllen o == bllen i - 1 }, Data.Word.Word8)

last
    :: { bs : Data.ByteString.Lazy.ByteString | 1 <= bllen bs }
    -> Data.Word.Word8

tail
    :: { bs : Data.ByteString.Lazy.ByteString | 1 <= bllen bs }
    -> Data.Word.Word8

init
    :: { bs : Data.ByteString.Lazy.ByteString | 1 <= bllen bs }
    -> Data.Word.Word8

null
    :: bs : Data.ByteString.Lazy.ByteString
    -> { b : GHC.Types.Bool | Prop b <=> bllen bs == 0 }

length
    :: bs : Data.ByteString.Lazy.ByteString
    -> { n : Nat64 | bllen bs == n }

map
    :: (Data.Word.Word8 -> Data.Word.Word8)
    -> i : Data.ByteString.Lazy.ByteString
    -> { o : Data.ByteString.Lazy.ByteString | bllen o == bllen i }

reverse
    :: i : Data.ByteString.Lazy.ByteString
    -> { o : Data.ByteString.Lazy.ByteString | bllen o == bllen i }

intersperse
    :: Data.Word.Word8
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
    :: (Data.Word.Word8 -> Data.Word.Word8 -> Data.Word.Word8)
    -> { bs : Data.ByteString.Lazy.ByteString | 1 <= bllen bs }
    -> Data.Word.Word8

foldl1'
    :: (Data.Word.Word8 -> Data.Word.Word8 -> Data.Word.Word8)
    -> { bs : Data.ByteString.Lazy.ByteString | 1 <= bllen bs }
    -> Data.Word.Word8

foldr1
    :: (Data.Word.Word8 -> Data.Word.Word8 -> Data.Word.Word8)
    -> { bs : Data.ByteString.Lazy.ByteString | 1 <= bllen bs }
    -> Data.Word.Word8

foldr1'
    :: (Data.Word.Word8 -> Data.Word.Word8 -> Data.Word.Word8)
    -> { bs : Data.ByteString.Lazy.ByteString | 1 <= bllen bs }
    -> Data.Word.Word8

concat
    :: is : [Data.ByteString.Lazy.ByteString]
    -> { o : Data.ByteString.Lazy.ByteString | len is == 0 ==> bllen o }

concatMap
    :: (Data.Word.Word8 -> Data.ByteString.Lazy.ByteString)
    -> i : Data.ByteString.Lazy.ByteString
    -> { o : Data.ByteString.Lazy.ByteString | bllen i == 0 ==> bllen o == 0 }

any
    :: (Data.Word.Word8 -> GHC.Types.Bool)
    -> bs : Data.ByteString.Lazy.ByteString
    -> { b : GHC.Types.Bool | bllen bs == 0 ==> not (Prop b) }

all
    :: (Data.Word.Word8 -> GHC.Types.Bool)
    -> bs : Data.ByteString.Lazy.ByteString
    -> { b : GHC.Types.Bool | bllen bs == 0 ==> Prop b }

maximum
    :: { bs : Data.ByteString.Lazy.ByteString | 1 <= bllen bs }
    -> Data.Word.Word8

minimum
    :: { bs : Data.ByteString.Lazy.ByteString | 1 <= bllen bs }
    -> Data.Word.Word8

scanl
    :: (Data.Word.Word8 -> Data.Word.Word8 -> Data.Word.Word8)
    -> Data.Word.Word8
    -> i : Data.ByteString.Lazy.ByteString
    -> { o : Data.ByteString.Lazy.ByteString | bllen o == bllen i }

scanl1
    :: (Data.Word.Word8 -> Data.Word.Word8 -> Data.Word.Word8)
    -> i : { i : Data.ByteString.Lazy.ByteString | 1 <= bllen i }
    -> { o : Data.ByteString.Lazy.ByteString | bllen o == bllen i }

scanr
    :: (Data.Word.Word8 -> Data.Word.Word8 -> Data.Word.Word8)
    -> Data.Word.Word8
    -> i : Data.ByteString.Lazy.ByteString
    -> { o : Data.ByteString.Lazy.ByteString | bllen o == bllen i }

scanr1
    :: (Data.Word.Word8 -> Data.Word.Word8 -> Data.Word.Word8)
    -> i : { i : Data.ByteString.Lazy.ByteString | 1 <= bllen i }
    -> { o : Data.ByteString.Lazy.ByteString | bllen o == bllen i }

mapAccumL
    :: (acc -> Data.Word.Word8 -> (acc, Data.Word.Word8))
    -> acc
    -> i : Data.ByteString.Lazy.ByteString
    -> (acc, { o : Data.ByteString.Lazy.ByteString | bllen o == bllen i })

mapAccumR
    :: (acc -> Data.Word.Word8 -> (acc, Data.Word.Word8))
    -> acc
    -> i : Data.ByteString.Lazy.ByteString
    -> (acc, { o : Data.ByteString.Lazy.ByteString | bllen o == bllen i })

replicate
    :: n : Data.Int.Int64
    -> Data.Word.Word8
    -> { bs : Data.ByteString.Lazy.ByteString | bllen bs == n }

unfoldrN
    :: n : GHC.Types.Int
    -> (a -> Data.Maybe.Maybe (Data.Word.Word8, a))
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
    :: (Data.Word.Word8 -> GHC.Types.Bool)
    -> i : Data.ByteString.Lazy.ByteString
    -> { o : Data.ByteString.Lazy.ByteString | bllen o <= bllen i }

dropWhile
    :: (Data.Word.Word8 -> GHC.Types.Bool)
    -> i : Data.ByteString.Lazy.ByteString
    -> { o : Data.ByteString.Lazy.ByteString | bllen o <= bllen i }

span
    :: (Data.Word.Word8 -> GHC.Types.Bool)
    -> i : Data.ByteString.Lazy.ByteString
    -> ( { l : Data.ByteString.Lazy.ByteString | bllen l <= bllen i }
       , { r : Data.ByteString.Lazy.ByteString | bllen r <= bllen i }
       )

spanEnd
    :: (Data.Word.Word8 -> GHC.Types.Bool)
    -> i : Data.ByteString.Lazy.ByteString
    -> ( { l : Data.ByteString.Lazy.ByteString | bllen l <= bllen i }
       , { r : Data.ByteString.Lazy.ByteString | bllen r <= bllen i }
       )

break
    :: (Data.Word.Word8 -> GHC.Types.Bool)
    -> i : Data.ByteString.Lazy.ByteString
    -> ( { l : Data.ByteString.Lazy.ByteString | bllen l <= bllen i }
       , { r : Data.ByteString.Lazy.ByteString | bllen r <= bllen i }
       )

breakEnd
    :: (Data.Word.Word8 -> GHC.Types.Bool)
    -> i : Data.ByteString.Lazy.ByteString
    -> ( { l : Data.ByteString.Lazy.ByteString | bllen l <= bllen i }
       , { r : Data.ByteString.Lazy.ByteString | bllen r <= bllen i }
       )

group
    :: i : Data.ByteString.Lazy.ByteString
    -> [{ o : Data.ByteString.Lazy.ByteString | 1 <= bllen o && bllen o <= bllen i }]

groupBy
    :: (Data.Word.Word8 -> Data.Word.Word8 -> GHC.Types.Bool)
    -> i : Data.ByteString.Lazy.ByteString
    -> [{ o : Data.ByteString.Lazy.ByteString | 1 <= bllen o && bllen o <= bllen i }]

inits
    :: i : Data.ByteString.Lazy.ByteString
    -> [{ o : Data.ByteString.Lazy.ByteString | bllen o <= bllen i }]

tails
    :: i : Data.ByteString.Lazy.ByteString
    -> [{ o : Data.ByteString.Lazy.ByteString | bllen o <= bllen i }]

split
    :: Data.Word.Word8
    -> i : Data.ByteString.Lazy.ByteString
    -> [{ o : Data.ByteString.Lazy.ByteString | bllen o <= bllen i }]

splitWith
    :: (Data.Word.Word8 -> GHC.Types.Bool)
    -> i : Data.ByteString.Lazy.ByteString
    -> [{ o : Data.ByteString.Lazy.ByteString | bllen o <= bllen i }]

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
    :: Data.Word.Word8
    -> bs : Data.ByteString.Lazy.ByteString
    -> { b : GHC.Types.Bool | bllen b == 0 ==> not (Prop b) }

notElem
    :: Data.Word.Word8
    -> bs : Data.ByteString.Lazy.ByteString
    -> { b : GHC.Types.Bool | bllen b == 0 ==> Prop b }

find
    :: (Data.Word.Word8 -> GHC.Types.Bool)
    -> bs : Data.ByteString.Lazy.ByteString
    -> Data.Maybe.Maybe { w8 : Data.Word.Word8 | bllen bs /= 0 }

filter
    :: (Data.Word.Word8 -> GHC.Types.Bool)
    -> i : Data.ByteString.Lazy.ByteString
    -> { o : Data.ByteString.Lazy.ByteString | bllen o <= bllen i }

partition
    :: (Data.Word.Word8 -> GHC.Types.Bool)
    -> i : Data.ByteString.Lazy.ByteString
    -> ( { l : Data.ByteString.Lazy.ByteString | bllen l <= bllen i }
       , { r : Data.ByteString.Lazy.ByteString | bllen r <= bllen i }
       )

index
    :: bs : Data.ByteString.Lazy.ByteString
    -> { n : Data.Int.Int64 | 0 <= n && n < bllen bs }
    -> Data.Word.Word8

elemIndex
    :: Data.Word.Word8
    -> bs : Data.ByteString.Lazy.ByteString
    -> Data.Maybe.Maybe { n : Data.Int.Int64 | 0 <= n && n < bllen bs }

elemIndices
    :: Data.Word.Word8
    -> bs : Data.ByteString.Lazy.ByteString
    -> [{ n : Data.Int.Int64 | 0 <= n && n < bllen bs }]

elemIndexEnd
    :: Data.Word.Word8
    -> bs : Data.ByteString.Lazy.ByteString
    -> Data.Maybe.Maybe { n : Data.Int.Int64 | 0 <= n && n < bllen bs }

findIndex
    :: (Data.Word.Word8 -> Bool)
    -> bs : Data.ByteString.Lazy.ByteString
    -> Data.Maybe.Maybe { n : Data.Int.Int64 | 0 <= n && n < bllen bs }

findIndices
    :: (Data.Word.Word8 -> Bool)
    -> bs : Data.ByteString.Lazy.ByteString
    -> [{ n : Data.Int.Int64 | 0 <= n && n < bllen bs }]

count
    :: Data.Word.Word8
    -> bs : Data.ByteString.Lazy.ByteString
    -> { n : Data.Int.Int64 | 0 <= n && n < bllen bs }

zip
    :: l : Data.ByteString.Lazy.ByteString
    -> r : Data.ByteString.Lazy.ByteString
    -> { o : [(Data.Word.Word8, Data.Word.Word8)] | len o <= bllen l && len o <= bllen r }

zipWith
    :: (Data.Word.Word8 -> Data.Word.Word8 -> a)
    -> l : Data.ByteString.Lazy.ByteString
    -> r : Data.ByteString.Lazy.ByteString
    -> { o : [a] | len o <= bllen l && len o <= bllen r }

unzip
    :: i : [(Data.Word.Word8, Data.Word.Word8)]
    -> ( { l : Data.ByteString.Lazy.ByteString | bllen l == len i }
       , { r : Data.ByteString.Lazy.ByteString | bllen r == len i }
       )

sort
    :: i : Data.ByteString.Lazy.ByteString
    -> { o : Data.ByteString.Lazy.ByteString | bllen o == bllen i }

copy
    :: i : Data.ByteString.Lazy.ByteString
    -> { o : Data.ByteString.Lazy.ByteString | bllen o == bllen i }

hGet
    :: System.IO.Handle
    -> n : { n : GHC.Types.Int | 0 <= n }
    -> GHC.Types.IO { bs : Data.ByteString.Lazy.ByteString | bllen bs == n || bllen bs == 0 }

hGetNonBlocking
    :: System.IO.Handle
    -> n : { n : GHC.Types.Int | 0 <= n }
    -> GHC.Types.IO { bs : Data.ByteString.Lazy.ByteString | bllen bs <= n }
