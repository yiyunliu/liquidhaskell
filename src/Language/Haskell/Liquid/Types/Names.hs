module Language.Haskell.Liquid.Types.Names
  (lenLocSymbol, anyTypeSymbol) where

import Language.Fixpoint.Types
import Language.Haskell.Liquid.Types.LHSymbol
import TysWiredIn (anyTyCon)

-- RJ: Please add docs
lenLocSymbol :: Located FixSymbol
lenLocSymbol = dummyLoc $ symbol ("autolen" :: String)

anyTypeSymbol :: LHSymbol
anyTypeSymbol = LHTyCon $ anyTyCon
