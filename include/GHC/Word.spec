module spec GHC.Word where

import GHC.Types

embed GHC.Word.Word8  as int
embed GHC.Word.Word16 as int
embed GHC.Word.Word32 as int
embed GHC.Word.Word64 as int
