{-# LANGUAGE TupleSections #-}

module Language.Haskell.Liquid.Spec (
    -- * Build Module Specs
    makeSpecs
    -- * Post-Process Module Specs
  , postProcessSpecs
  ) where

import GHC hiding (Located)

import CoreSyn
import NameSet
import Var

import Control.Exception

import qualified Data.HashMap.Strict as M

import Language.Fixpoint.Types

import Language.Haskell.Liquid.GHC.Misc
import Language.Haskell.Liquid.Types
import Language.Haskell.Liquid.Types.RefType

import qualified Language.Haskell.Liquid.Measure as Ms

import Language.Haskell.Liquid.Spec.Alias
import Language.Haskell.Liquid.Spec.DataType
import Language.Haskell.Liquid.Spec.Env
import Language.Haskell.Liquid.Spec.Lookup
import Language.Haskell.Liquid.Spec.Measure
import Language.Haskell.Liquid.Spec.Resolve

--------------------------------------------------------------------------------
-- | Build Module Specs --------------------------------------------------------
--------------------------------------------------------------------------------

makeSpecs :: Config
          -> GlobalSpec
          -> CoreProgram
          -> [Var]
          -> [Var]
          -> NameSet
          -> Maybe Module
          -> Ms.BareSpec
          -> Ghc (GlobalSpec, LocalSpec)
makeSpecs cfg extern cbs vars lvars exports mod bspec =
  either throw return =<<
    runSpecM (makeSpecs' cfg cbs vars lvars exports mod bspec) extern bspec
  
makeSpecs' :: Config
           -> CoreProgram
           -> [Var]
           -> [Var]
           -> NameSet
           -> Maybe Module
           -> Ms.BareSpec
           -> SpecM (GlobalSpec, LocalSpec)
makeSpecs' _cfg _cbs _vars _lvars _exports _mod bspec = do
  tcEmbeds           <- makeTyConEmbeds bspec
  aliases            <- makeAliases bspec
  withLocalAliases aliases $ do
  (tyconEnv, dconsP) <- makeDataTypes bspec
  varianceEnv        <- makeVarianceEnv bspec
  meas               <- makeMeasures bspec dconsP
  invariants         <- makeInvariants bspec
  ialiases           <- makeIAliases bspec
  qualifiers         <- makeQualifiers bspec
  return $
    ( emptyGlobalSpec
      { aliases     = aliases
      , meas        = meas
      , invariants  = invariants
      , ialiases    = ialiases
      , tcEmbeds    = tcEmbeds
      , qualifiers  = qualifiers
      , tyconEnv    = tyconEnv
      , varianceEnv = varianceEnv
      }
    , emptyLocalSpec
      { dconsP = dconsP
      }
    )

--------------------------------------------------------------------------------
-- | Make Simple Specifications ------------------------------------------------
--------------------------------------------------------------------------------

-- | Make Embedded TyCon Environment -------------------------------------------

makeTyConEmbeds :: Ms.BareSpec -> SpecM (TCEmb TyCon)
makeTyConEmbeds = fmap M.fromList . mapM go . M.toList . Ms.embeds
  where
    go (c, y) = (,y) <$> lookupGhcTyConL c

-- | Make Invariants & Invariant Aliases ---------------------------------------

makeInvariants :: Ms.BareSpec -> SpecM [(Maybe Var, LocSpecType)]
makeInvariants = mapM (fmap (Nothing,) . makeInvariantTy) . Ms.invariants

makeIAliases :: Ms.BareSpec -> SpecM [(LocSpecType, LocSpecType)]
makeIAliases = mapM go . Ms.ialiases
  where
    go (t1, t2) = (,) <$> makeInvariantTy t1 <*> makeInvariantTy t2

makeInvariantTy :: Located BareType -> SpecM LocSpecType
makeInvariantTy = fmap (fmap generalize) . resolveL'

-- | Make Qualifiers -----------------------------------------------------------

makeQualifiers :: Ms.BareSpec -> SpecM (M.HashMap Symbol Qualifier)
makeQualifiers = fmap M.fromList . mapM go . Ms.qualifiers
  where
    go q = (qName q,) <$> resolve (sourcePosSrcSpan $ qPos q) q

--------------------------------------------------------------------------------
-- | Post-Process Module Specs -------------------------------------------------
--------------------------------------------------------------------------------

postProcessSpecs :: GlobalSpec -> LocalSpec -> Ghc (GlobalSpec, LocalSpec)
postProcessSpecs gbl lcl = return (gbl', lcl)
  where
    gbl' = applyVarianceInfo gbl

