module Language.Haskell.Liquid.Types.PrettyPrint.Instances where

import           Language.Fixpoint.Types.PrettyPrint
import           HscTypes                         (SourceError)
import           ErrUtils                         (ErrMsg)
import           Var
import           Name
import           TyCon
import           Type
import           Class
import           Text.PrettyPrint.HughesPJ
import           Language.Haskell.Liquid.GHC.Misc.PrettyPrint


--------------------------------------------------------------------------------
-- | A whole bunch of PPrint instances follow ----------------------------------
--------------------------------------------------------------------------------
instance PPrint ErrMsg where
  pprintTidy _ = text . show

instance PPrint SourceError where
  pprintTidy _ = text . show

instance PPrint Var where
  pprintTidy _ = pprDoc

instance PPrint Name where
  pprintTidy _ = pprDoc

shortModules :: a -> a
shortModules = id

instance PPrint TyCon where
  pprintTidy Lossy = shortModules . pprDoc
  pprintTidy Full  =                pprDoc

instance PPrint Type where
  pprintTidy _ = pprDoc -- . tidyType emptyTidyEnv -- WHY WOULD YOU DO THIS???

instance PPrint Class where
  pprintTidy Lossy = shortModules . pprDoc
  pprintTidy Full  =                pprDoc


