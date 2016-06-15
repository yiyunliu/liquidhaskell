{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}

module Language.Haskell.Liquid.Spec.DataType (
    -- * Assemble Data Type Specifications
    makeDataTypes
  , makeVarianceEnv
    -- * Apply Variance Specifications
  , applyVarianceInfo
  ) where

import GHC hiding (Located)

import DataCon

import Control.Monad.State

import Data.List

import qualified Data.HashMap.Strict as M

import Language.Fixpoint.Types

import Language.Haskell.Liquid.GHC.Misc
import Language.Haskell.Liquid.Misc
import Language.Haskell.Liquid.Types
import Language.Haskell.Liquid.Types.RefType
import Language.Haskell.Liquid.Types.Variance
import Language.Haskell.Liquid.WiredIn

import qualified Language.Haskell.Liquid.Measure as Ms

import Language.Haskell.Liquid.Spec.Env
import Language.Haskell.Liquid.Spec.Lookup
import Language.Haskell.Liquid.Spec.Resolve

--------------------------------------------------------------------------------
-- | Assemble Data Type Specifications -----------------------------------------
--------------------------------------------------------------------------------

makeDataTypes :: Ms.BareSpec
              -> SpecM (TCEnv, [(DataCon, DataConP)])
makeDataTypes bspec = do
  (tcs, dcs) <- unzip <$> mapM makeDataType (Ms.dataDecls bspec)
  return (M.fromList tcs, concat dcs)

makeDataType :: Located DataDecl
             -> SpecM ((TyCon, Located RTyCon), [(DataCon, DataConP)])
makeDataType (Loc l l' (D tc as ps ls cts _ sfun)) = do
  tc' <- lookupGhcTyConL tc
  -- FIXME: symbolRTyVar is probably the wrong way to do this
  let as' = symbolRTyVar <$> as
  ps' <- mapM resolveL ps
  cts' <- mapM (makeDataCon tc' as' ps' ls) cts
  let rtc = makeRTyCon tc' as' ps' sfun (snd <$> cts')
  return ((tc', Loc l l' rtc), cts')


makeVarianceEnv :: Ms.BareSpec -> SpecM VarianceEnv
makeVarianceEnv =
  (M.fromList <$>) . mapM (firstM lookupGhcTyConL) . Ms.dvariance

--------------------------------------------------------------------------------
-- | Make Data Constructor Specifications --------------------------------------
--------------------------------------------------------------------------------

makeDataCon :: TyCon
            -> [RTyVar]
            -> [RPVar]
            -> [Symbol]
            -> (LocSymbol, [(LocSymbol, LocBareType)])
            -> SpecM (DataCon, DataConP)
makeDataCon tc as ps ls (c, xts) = do
  c' <- lookupGhcDataConL c
  let (xs, ts) = unzip xts
  ts' <- mapM (resolvePL ps) ts
  let xts' = reverse $ zip xs ts'
  let cs = ofType <$> dataConStupidTheta c'
  let rs = rVar . rtv_tv <$> as
  let t0 = rApp tc rs (rPropP [] . pdVarReft <$> ps) mempty
  return (c', DataConP (loc c) as ps ls cs xts' t0 (locE c))

--------------------------------------------------------------------------------
-- | Make RTyCon Environment ---------------------------------------------------
--------------------------------------------------------------------------------

makeRTyCon :: TyCon
           -> [RTyVar]
           -> [RPVar]
           -> Maybe (Symbol -> Expr)
           -> [DataConP]
           -> RTyCon
makeRTyCon tc as ps sfun cts =
  RTyCon tc ps' $ mkTyConInfo tc [] pvi sfun
  where
    ps' = subts (zip as as') <$> ps
    as' = rVar <$> tyConTyVarsDef tc :: [RSort]
    pvi = derivePVarVariance ps tys
    tys = concatMap ((snd <$>) . tyArgs) cts

derivePVarVariance :: [RPVar] -> [SpecType] -> VarianceInfo
derivePVarVariance ps tys =
  snd <$> execState (mapM_ (go_ty [] True) tys) initState
  where
    initState = (\p -> (uPVar p, Invariant)) <$> ps

    go_ty pvs pos (RVar _ r) =
      go_rf pvs pos r
    go_ty pvs pos (RFun _ i o r) = do
      go_ty pvs (not pos) i
      go_ty pvs pos o
      go_rf pvs pos r
    go_ty pvs pos (RAllT _ t) =
      go_ty pvs pos t
    go_ty pvs pos (RAllP p t) =
      go_ty (uPVar p : pvs) pos t
    go_ty pvs pos (RAllS _ t) =
      go_ty pvs pos t
    go_ty pvs pos (RApp _ ts rs r) = do
      mapM_ (go_ty pvs pos) ts
      mapM_ (go_rp pvs pos) rs
      go_rf pvs pos r
    go_ty pvs pos (RAllE _ a t) = do
      go_ty pvs pos a
      go_ty pvs pos t
    go_ty pvs pos (REx _ e t) = do
      go_ty pvs pos e
      go_ty pvs pos t
    go_ty _ _ (RExprArg _) =
      return ()
    go_ty pvs pos (RAppTy t1 t2 r) = do
      go_ty pvs pos t1
      go_ty pvs pos t2
      go_rf pvs pos r
    go_ty pvs pos (RRTy env r _ t) = do
      mapM_ (go_ty pvs pos) $ snd <$> env
      go_rf pvs pos r
      go_ty pvs pos t
    go_ty pvs pos (RHole r) =
      go_rf pvs pos r

    go_rp pvs pos (RProp _ (RHole r)) =
      go_rf pvs pos r
    go_rp pvs pos (RProp _ t) =
      go_ty pvs pos t

    go_rf pvs pos (MkUReft _ ps _) =
      mapM_ (go_pv pvs pos) $ pvars ps

    go_pv pvs pos p
      | p `elem` pvs = return ()
      | otherwise = modify (updateVariance pos p <$>)

    updateVariance pos p (p', var) =
      (p',) $ if | p /= p' -> var
                 | var /= Invariant -> Bivariant
                 | pos -> Covariant
                 | otherwise -> Contravariant

--------------------------------------------------------------------------------
-- | Apply Variance Specifications ---------------------------------------------
--------------------------------------------------------------------------------

applyVarianceInfo :: GlobalSpec -> GlobalSpec
applyVarianceInfo bspec = bspec { tyconEnv = tyconEnv' }
  where
    tyconEnv' =
      foldl' applyVarianceInfo' (tyconEnv bspec) $ M.toList $ varianceEnv bspec

applyVarianceInfo' :: TCEnv -> (TyCon, Located VarianceInfo) -> TCEnv
applyVarianceInfo' tyi (tc, Loc _ _ vs) =
  M.insert tc (updateVarianceInfo tc vs <$> rtc) tyi
  where
    rtc = M.lookupDefault (dummyLoc $ rTyCon tc) tc tyi

updateVarianceInfo :: TyCon -> VarianceInfo -> RTyCon -> RTyCon
updateVarianceInfo c vs rtc = rtc
  { rtc_info = tci
    { varianceTyArgs = tvs
    , variancePsArgs = if null pvs then variancePsArgs tci else pvs
    }
  }
  where
    tci = rtc_info rtc
    tvs = take n vs
    pvs = drop n vs
    n   = length $ tyConTyVarsDef c

