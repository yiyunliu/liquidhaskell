module spec Data.ByteString.Unsafe where

import Data.ByteString

import GHC.Base

unsafeHead
    :: { bs : Data.ByteString.ByteString | 1 <= bslen bs } -> Data.Word.Word8

unsafeTail
    :: { bs : Data.ByteString.ByteString | 1 <= bslen bs } -> Data.Word.Word8

unsafeInit
    :: { bs : Data.ByteString.ByteString | 1 <= bslen bs } -> Data.Word.Word8

unsafeLast
    :: { bs : Data.ByteString.ByteString | 1 <= bslen bs } -> Data.Word.Word8

unsafeIndex
    :: bs : Data.ByteString.ByteString
    -> { n : GHC.Types.Int | 0 <= n && n < bslen bs }
    -> Data.Word.Word8

unsafeTake
    :: n : { n : GHC.Types.Int | 0 <= n }
    -> i : { i : Data.ByteString.ByteString | n <= bslen i }
    -> { o : Data.ByteString.ByteString | bslen o == n }

unsafeDrop
    :: n : { n : GHC.Types.Int | 0 <= n }
    -> i : { i : Data.ByteString.ByteString | n <= bslen i }
    -> { o : Data.ByteString.ByteString | bslen o == bslen i - n }
