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
import           Data.Data

-----------------------------------------------------------------------------
-- | GHC Specific Symbol
-----------------------------------------------------------------------------

data LHSymbol =
    LHName Name -- ^ placeholder
  | LHVar Var -- ^ placeholder
  | LHRefSym FixSymbol -- ^ placeholder
  deriving (Eq, Ord, Generic, Data)


instance Hashable LHSymbol

instance PPrint LHSymbol where
  pprintTidy op (LHName name)  = pprintTidy op name
  pprintTidy op (LHVar var)  = pprintTidy op var

instance NFData LHSymbol


instance Binary Name where
  put = undefined
  get = undefined

instance Binary Var where
  put = undefined
  get = undefined

instance Binary LHSymbol

-- should be defined based on Name
instance SMTLIB2 LHSymbol LHSymbol where
  smt2 env s
    | Just t <- Thy.smt2Symbol env (AS s) = t
  smt2 _ _  = undefined

instance Fixpoint LHSymbol where
  toFix (LHName name) = toFix name
  toFix (LHVar var) = toFix var

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
