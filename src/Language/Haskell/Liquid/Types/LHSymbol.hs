-- | This module defines the type used to instantiate the
-- | abstract "s" in the refactored liquid-fixpoint

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TemplateHaskell       #-}




module Language.Haskell.Liquid.Types.LHSymbol
  ( LHSymbol (..)
  , lhrefBind
  , substSymbolS
  , lhRefSym
  , lhVar
  , lhTyCon
  , lhDataCon
  ) where

import           Name
import           GHC.Generics              (Generic)
import           Language.Fixpoint.Smt.Types
import           Language.Fixpoint.Types   (Symbol(..), PPrint(..), Fixpoint(..), FixSymbol(..), Expr(..))
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
import           Lens.Micro.Platform

-----------------------------------------------------------------------------
-- | GHC Specific Symbol
-----------------------------------------------------------------------------

data LHSymbol =
    LHDataCon {_lhDataCon :: DataCon} 
  | LHTyCon {_lhTyCon :: TyCon}
  | LHVar {_lhVar :: Var} -- ^ placeholder
  | LHRefSym {_lhRefSym :: FixSymbol} -- ^ placeholder
  deriving (Eq, Generic, Data)


makeLenses ''LHSymbol

-- YL: Shouldn't generalize too soon. Think about what information a lens/function knows about 
-- encoding the knowledge of going to the "default" expression
-- LHSymbol -> Symbol LHSymbol -> Wrapped in EVar
-- | ignoring the AS branch
substSymbolS :: Traversal (Symbol LHSymbol) (Expr LHSymbol) FixSymbol (Expr LHSymbol)
substSymbolS f (AS (LHRefSym s)) = f s
substSymbolS _ t = pure (EVar t)



lhrefBind :: Traversal LHSymbol LHSymbol FixSymbol LHSymbol
lhrefBind f (LHRefSym s) = f s
lhrefBind _ s            = pure s

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
  show (LHRefSym x) = show x
  show _ = undefined


  

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
