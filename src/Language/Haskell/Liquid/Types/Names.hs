module Language.Haskell.Liquid.Types.Names
  (lenLocSymbol, anyTypeSymbol) where

import Language.Fixpoint.Types
import Language.Haskell.Liquid.Types.LHSymbol
import Name (getName)
import TysWiredIn (anyTyCon)

-- RJ: Please add docs
lenLocSymbol :: Located (Symbol LHSymbol)
lenLocSymbol = dummyLoc . FS $ symbol ("autolen" :: String)

anyTypeSymbol :: Symbol LHSymbol
anyTypeSymbol = AS . LHTyCon $ anyTyCon
