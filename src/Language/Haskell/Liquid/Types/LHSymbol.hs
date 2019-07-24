-- | This module defines the type used to instantiate the
-- | abstract "s" in the refactored liquid-fixpoint

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}




module Language.Haskell.Liquid.Types.LHSymbol
  ( LHSymbol (..)
  ) where

import           Name
import           GHC.Generics              (Generic)
import           Language.Fixpoint.Smt.Types
import           Language.Fixpoint.Types   (Symbol(..), PPrint(..), Fixpoint(..), FixSymbol(..))
import qualified Language.Fixpoint.Smt.Theories as Thy
import           Language.Haskell.Liquid.Types.PrettyPrint.Instances ()
import           Data.Hashable
import           Outputable                                 ()
import           Data.Binary
import           Var
import           Control.DeepSeq
import           Data.Data                                  (Data)
import           DataCon
import           TyCon

-----------------------------------------------------------------------------
-- | GHC Specific Symbol
-----------------------------------------------------------------------------

data LHSymbol =
    LHDataCon DataCon
  | LHTyCon TyCon
  | LHVar Var -- ^ placeholder
  | LHRefSym FixSymbol -- ^ placeholder
  deriving (Eq, Generic, Data)

instance Ord LHSymbol where
  compare = undefined

instance Hashable LHSymbol where
  hashWithSalt = undefined

instance PPrint LHSymbol where
  pprintTidy op (LHVar var)  = pprintTidy op var
  pprintTidy _ _ = undefined

instance NFData LHSymbol where
  rnf = undefined


instance Binary Name where
  put = undefined
  get = undefined

instance Binary Var where
  put = undefined
  get = undefined

instance Binary LHSymbol where
  put = undefined
  get = undefined

-- should be defined based on Name
instance SMTLIB2 LHSymbol LHSymbol where
  smt2 env s
    | Just t <- Thy.smt2Symbol env (AS s) = t
  smt2 _ _  = undefined

instance Fixpoint LHSymbol where
  toFix (LHVar var) = toFix var
  toFix _ = undefined

instance Show LHSymbol where
  show = undefined


  

-- those orphan instances are difficult to use

-- sDocDoc :: Out.SDoc -> PJ.Doc
-- sDocDoc   = PJ.text . showSDoc

-- pprDoc :: Outputable a => a -> PJ.Doc
-- pprDoc    = sDocDoc . ppr

-- showSDoc :: Out.SDoc -> String
-- showSDoc sdoc = Out.renderWithStyle unsafeGlobalDynFlags sdoc (Out.mkUserStyle unsafeGlobalDynFlags myQualify {- Out.alwaysQualify -} Out.AllTheWay)

-- myQualify :: Out.PrintUnqualified
-- myQualify = Out.neverQualify { Out.queryQualifyName = Out.alwaysQualifyNames }
-- -- { Out.queryQualifyName = \_ _ -> Out.NameNotInScope1 }
