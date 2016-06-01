{-# LANGUAGE TupleSections #-}

module Language.Haskell.Liquid.Spec.Measure (
    -- * Make Measures
    makeMeasures
  ) where

import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S

import Language.Fixpoint.Types

import Language.Haskell.Liquid.Types

import qualified Language.Haskell.Liquid.Measure as Ms

import Language.Haskell.Liquid.Spec.Env

--------------------------------------------------------------------------------
-- | Make Measures -------------------------------------------------------------
--------------------------------------------------------------------------------

makeMeasures :: Ms.BareSpec -> SpecM (M.HashMap Symbol LocSpecType)
makeMeasures bspec = do
  plainMeas <- mapM makePlainMeasure $ Ms.measures  bspec
  classMeas <- mapM makeClassMeasure $ Ms.cmeasures bspec
  instMeas  <- mapM makeInstMeasure  $ Ms.imeasures bspec
  haskMeas  <- mapM makeHaskMeasure  $ S.toList $ Ms.hmeas bspec
  return $ M.fromList $ concat [plainMeas, classMeas, instMeas, haskMeas]

-- TODO: Placeholder for name collection/resolution
makePlainMeasure :: Measure (Located BareType) LocSymbol
                 -> SpecM (Symbol, LocSpecType)
makePlainMeasure measure = return
  ( val $ name measure
  , const (RHole mempty) <$> name measure
  )

-- TODO: Placeholder for name collection/resolution
makeClassMeasure :: Measure (Located BareType) ()
                 -> SpecM (Symbol, LocSpecType)
makeClassMeasure measure = return
  ( val $ name measure
  , const (RHole mempty) <$> name measure
  )

-- TODO: Placeholder for name collection/resolution
makeInstMeasure :: Measure (Located BareType) LocSymbol
                -> SpecM (Symbol, LocSpecType)
makeInstMeasure measure = return
  ( val $ name measure
  , const (RHole mempty) <$> name measure
  )

-- TODO: Placeholder for name collection/resolution
makeHaskMeasure :: LocSymbol -> SpecM (Symbol, LocSpecType)
makeHaskMeasure name = return
  ( val name
  , const (RHole mempty) <$> name
  )

