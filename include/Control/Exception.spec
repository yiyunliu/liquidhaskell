module spec Control.Exception where

import GHC.Types

// Useless as compiled into GHC primitive, which is ignored
assume assert
  :: {v:GHC.Types.Bool | Prop v}
  -> a
  -> a
