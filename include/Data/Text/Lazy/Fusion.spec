module spec Data.Text.Lazy.Fusion where

import GHC.Base

// FIXME: `ltlength` is missing!

stream
    :: t:Data.Text.Lazy.Internal.Text
    -> {v:Data.Text.Fusion.Internal.Stream GHC.Types.Char | (slen v) = (ltlength t)}

unstream
    :: s:Data.Text.Fusion.Internal.Stream GHC.Types.Char
    -> {v:Data.Text.Lazy.Internal.Text | (ltlength v) = (slen s)}

length
    :: s:Data.Text.Fusion.Internal.Stream GHC.Types.Char
    -> {v:GHC.Int.Int64 | v = (slen s)}
