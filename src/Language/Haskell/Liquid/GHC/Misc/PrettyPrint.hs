module Language.Haskell.Liquid.GHC.Misc.PrettyPrint where

import           Outputable                                 (Outputable (..), text, ppr)
import qualified Outputable                                 as Out
import           Language.Fixpoint.Types                    hiding (L, panic, Loc (..), SrcSpan, Constant, SESearch (..))
import qualified Text.PrettyPrint.HughesPJ                  as PJ
import qualified Data.HashSet                               as S
import           Debug.Trace
import           Data.Hashable
import           DynFlags
import           Control.DeepSeq
import           Var
import           TyCon
import           Type
import           Class
import           Name
import           DataCon
import           Unique


--------------------------------------------------------------------------------
-- | Pretty Printers -----------------------------------------------------------
--------------------------------------------------------------------------------
notracePpr :: Outputable a => String -> a -> a
notracePpr _ x = x

tracePpr :: Outputable a => String -> a -> a
tracePpr s x = trace ("\nTrace: [" ++ s ++ "] : " ++ showPpr x) x

pprShow :: Show a => a -> Out.SDoc
pprShow = text . show


toFixSDoc :: Fixpoint a => a -> PJ.Doc
toFixSDoc = PJ.text . PJ.render . toFix

sDocDoc :: Out.SDoc -> PJ.Doc
sDocDoc   = PJ.text . showSDoc

pprDoc :: Outputable a => a -> PJ.Doc
pprDoc    = sDocDoc . ppr

-- Overriding Outputable functions because they now require DynFlags!
showPpr :: Outputable a => a -> String
showPpr       = showSDoc . ppr

-- FIXME: somewhere we depend on this printing out all GHC entities with
-- fully-qualified names...
showSDoc :: Out.SDoc -> String
showSDoc sdoc = Out.renderWithStyle unsafeGlobalDynFlags sdoc (Out.mkUserStyle unsafeGlobalDynFlags myQualify {- Out.alwaysQualify -} Out.AllTheWay)

myQualify :: Out.PrintUnqualified
myQualify = Out.neverQualify { Out.queryQualifyName = Out.alwaysQualifyNames }
-- { Out.queryQualifyName = \_ _ -> Out.NameNotInScope1 }

showSDocDump :: Out.SDoc -> String
showSDocDump  = Out.showSDocDump unsafeGlobalDynFlags

instance Outputable a => Outputable (S.HashSet a) where
  ppr = ppr . S.toList

typeUniqueString :: Outputable a => a -> String
typeUniqueString = {- ("sort_" ++) . -} showSDocDump . ppr

instance Hashable Name where
  hashWithSalt = uniqueHash

instance Hashable Var where
  hashWithSalt = uniqueHash

instance Hashable TyCon where
  hashWithSalt = uniqueHash

instance Hashable DataCon where
  hashWithSalt = uniqueHash

instance Fixpoint Var where
  toFix = pprDoc

instance Fixpoint Name where
  toFix = pprDoc

instance Fixpoint Type where
  toFix = pprDoc

instance Show Name where
  -- show = show . symbol
  show = showPpr

instance Show Var where
  show = show . getName

instance Show Class where
  show = show . getName

instance Show TyCon where
  show = show . getName

instance NFData Class where
  rnf t = seq t ()

instance NFData TyCon where
  rnf t = seq t ()

instance NFData Type where
  rnf t = seq t ()

instance NFData Var where
  rnf t = seq t ()

uniqueHash :: Uniquable a => Int -> a -> Int
uniqueHash i = hashWithSalt i . getKey . getUnique

-- instance FixSymbolic TyCon where
--   symbol = symbol . getName

-- instance FixSymbolic Class where
--   symbol = symbol . getName

