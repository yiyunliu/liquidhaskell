{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE PatternGuards             #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams            #-}
{-# LANGUAGE PartialTypeSignatures     #-}

-- | This module defines the representation for Environments needed
--   during constraint generation.

module Language.Haskell.Liquid.Constraint.Env (

  -- * Insert
    (+++=)
  -- , (++=)
  , (+=)
  , extendEnvWithVV
  , addBinders
  , addSEnv
  , addEEnv
  , (-=)
  , globalize

  -- * Construction
  , fromListREnv
  , toListREnv
  , insertREnv -- TODO: remove this ASAP

  -- * Query
  , localBindsOfType
  , lookupREnv
  , (?=)

  -- * Pruning refinements (TODO: move!)
 , rTypeSortedReft'

  -- * Extend CGEnv
 , setLocation, setBind, setRecs, setTRec

  -- * Lookup CGEnv
 , getLocation

) where


-- import Name (getSrcSpan)
import Prelude hiding (error)
import CoreSyn (CoreExpr)
import SrcLoc
import Var
-- import Outputable
-- import FastString (fsLit)
import Control.Monad.State

-- import           GHC.Err.Located hiding (error)
import           GHC.Stack

import           Control.Arrow           (first)
import           Data.Maybe              -- (fromMaybe)
import qualified Data.List               as L
import qualified Data.HashSet            as S
import qualified Data.HashMap.Strict     as M
import qualified Language.Fixpoint.Types as F


import           Language.Fixpoint.SortCheck (pruneUnsortedReft)



import           Language.Haskell.Liquid.Types.RefType
import qualified Language.Haskell.Liquid.GHC.SpanStack as Sp
import           Language.Haskell.Liquid.Types            hiding (binds, Loc, loc, freeTyVars, Def)
import           Language.Haskell.Liquid.Types.LHSymbol
import           Language.Haskell.Liquid.Constraint.Types
import           Language.Haskell.Liquid.Constraint.Fresh ()
import           Language.Haskell.Liquid.Transforms.RefSplit
import qualified Language.Haskell.Liquid.UX.CTags       as Tg

-- import Debug.Trace (trace)
--------------------------------------------------------------------------------
-- | Refinement Type Environments ----------------------------------------------
--------------------------------------------------------------------------------

-- updREnvLocal :: REnv -> (_ -> _) -> REnv
updREnvLocal :: REnv
             -> (M.HashMap (F.Symbol LHSymbol) SpecType -> M.HashMap (F.Symbol LHSymbol) SpecType)
             -> REnv
updREnvLocal rE f      = rE { reLocal = f (reLocal rE) }

-- RJ: REnv-Split-Bug?
filterREnv :: (SpecType -> Bool) -> REnv -> REnv
filterREnv f rE        = rE `updREnvLocal` (M.filter f)


-- YL: Why is this made a String -> SpecType map? 
fromListREnv :: [(F.Symbol LHSymbol, SpecType)] -> [(F.Symbol LHSymbol, SpecType)] -> REnv
fromListREnv gXts lXts = REnv
  { reGlobal = M.fromList gXts
  , reLocal  = M.fromList lXts
  }

-- RJ: REnv-Split-Bug?
deleteREnv :: F.Symbol LHSymbol -> REnv -> REnv
deleteREnv x rE = rE `updREnvLocal` (M.delete x)

insertREnv :: F.Symbol LHSymbol -> SpecType -> REnv -> REnv
insertREnv x y rE = {- trace ("insertREnv: " ++ show x) $ -} rE `updREnvLocal` (M.insert x y)

-- YL: I would assume that lookupREnv happens in Expr?
lookupREnv :: F.Symbol LHSymbol -> REnv -> Maybe SpecType
lookupREnv x rE = msum $ M.lookup x <$> renvMaps rE

memberREnv :: F.Symbol LHSymbol -> REnv -> Bool
memberREnv x rE = or   $ M.member x <$> renvMaps rE

globalREnv :: REnv -> REnv
globalREnv (REnv gM lM) = REnv gM' M.empty
  where
    gM'  = M.unionWith (\_ t -> t) gM lM

renvMaps :: REnv -> [M.HashMap (F.Symbol LHSymbol) SpecType]
renvMaps rE = [reLocal rE, reGlobal rE]

--------------------------------------------------------------------------------
localBindsOfType :: RRType r  -> REnv -> [F.Symbol LHSymbol]
--------------------------------------------------------------------------------
localBindsOfType tx γ = fst <$> localsREnv (filterREnv ((== toRSort tx) . toRSort) γ)

-- RJ: REnv-Split-Bug?
-- YL: the local vs global renv is actually used for resolution???
localsREnv :: REnv -> [(F.Symbol LHSymbol, SpecType)]
localsREnv = M.toList . reLocal

globalsREnv :: REnv -> [(F.Symbol LHSymbol, SpecType)]
globalsREnv = M.toList . reGlobal

toListREnv :: REnv -> [(F.Symbol LHSymbol, SpecType)]
toListREnv re = globalsREnv re ++ localsREnv re

--------------------------------------------------------------------------------
extendEnvWithVV :: CGEnv -> SpecType -> CG CGEnv
--------------------------------------------------------------------------------
extendEnvWithVV γ t
  -- YL : do a pattern matching
  | F.isNontrivialVV undefined -- vv
    && not (vv `memberREnv` (renv γ))
  = γ += ("extVV", vv, t)
  | otherwise
  = return γ
  where
    vv = rTypeValueVar t

-- YL : binders could be from Var
addBinders :: CGEnv -> F.Symbol LHSymbol -> [(F.Symbol LHSymbol, SpecType)] -> CG CGEnv
addBinders γ0 x' cbs   = foldM (++=) (γ0 -= x') [("addBinders", x, t) | (x, t) <- cbs]

addBind :: SrcSpan -> F.Symbol LHSymbol -> F.SortedReft LHSymbol -> CG ((F.Symbol LHSymbol, F.Sort LHSymbol), F.BindId)
addBind l x r = do
  st          <- get
  let (i, bs') = F.insertBindEnv x r (binds st)
  put          $ st { binds = bs' } { bindSpans = M.insert i l (bindSpans st) }
  return ((x, F.sr_sort r), {- traceShow ("addBind: " ++ showpp x) -} i)

addClassBind :: CGEnv -> SrcSpan -> SpecType -> CG [((F.Symbol LHSymbol, F.Sort LHSymbol), F.BindId)]
addClassBind γ l = mapM (uncurry (addBind l)) . classBinds (emb γ)

{- see tests/pos/polyfun for why you need everything in fixenv -}
addCGEnv :: (SpecType -> SpecType) -> CGEnv -> (String, F.Symbol LHSymbol, SpecType) -> CG CGEnv
addCGEnv tx γ (eMsg, x, REx y tyy tyx) = do
  y' <- fresh
  γ' <- addCGEnv tx γ (eMsg, y', tyy)
  addCGEnv tx γ' (eMsg, x, tyx `F.subst1` (y, F.EVar y'))

addCGEnv tx γ (eMsg, x, RAllE yy tyy tyx)
  = addCGEnv tx γ (eMsg, x, t)
  where
    xs            = localBindsOfType tyy (renv γ)
    t             = L.foldl' (F.meet @LHSymbol) ttrue [ tyx' `F.subst1` (yy, F.EVar x) | x <- xs]
    (tyx', ttrue) = splitXRelatedRefs yy tyx

addCGEnv tx γ (_, x, t') = do
  idx   <- fresh
  -- allowHOBinders <- allowHO <$> get
  let t  = tx $ normalize idx t'
  let l  = getLocation γ
  let γ' = γ { renv = insertREnv x t (renv γ) }
  tem   <- getTemplates 
  is    <- (:) <$> addBind l x (rTypeSortedReft' γ' tem t) <*> addClassBind γ' l t
  return $ γ' { fenv = insertsFEnv (fenv γ) is }

rTypeSortedReft' :: (PPrint r, F.Reftable LHSymbol r, SubsTy RTyVar RSort r, F.Reftable LHSymbol (RTProp RTyCon RTyVar r))
                 => CGEnv -> F.Templates LHSymbol -> RRType r -> F.SortedReft LHSymbol
rTypeSortedReft' γ t 
  = pruneUnsortedReft (feEnv $ fenv γ) t . f
  where
    f         = rTypeSortedReft (emb γ)

normalize :: Integer -> SpecType -> SpecType
normalize idx = normalizeVV idx . normalizePds

normalizeVV :: Integer -> SpecType -> SpecType
normalizeVV idx t@(RApp _ _ _ _)
-- YL : pattern match >> shift >> then put it back
  | not (F.isNontrivialVV undefined -- (rTypeValueVar t)
        )
  = undefined -- shiftVV t (F.vv $ Just idx)

normalizeVV _ t
  = t

--------------------------------------------------------------------------------
(+=) :: CGEnv -> (String, F.Symbol LHSymbol, SpecType) -> CG CGEnv
--------------------------------------------------------------------------------
γ += (eMsg, x, r)
  -- YL: comparison, pattern matching
  | x ==  undefined -- F.dummySymbol
  = return γ
  -- // | x `memberREnv` (renv γ)
  -- // = _dupBindErr x γ
  | otherwise
  =  γ ++= (eMsg, x, r)

_dupBindError :: String -> F.Symbol LHSymbol -> CGEnv -> SpecType -> a
_dupBindError eMsg x γ r = panic Nothing s
  where
    -- YL : human readable symbol text. a generic symbolstring function that includes the Symbol s case?
    
    s = unlines [ eMsg ++ " Duplicate binding for " ++ undefined -- F.symbolString x
                , "   New: " ++ showpp r
                , "   Old: " ++ showpp (x `lookupREnv` (renv γ)) ]

--------------------------------------------------------------------------------
globalize :: CGEnv -> CGEnv
--------------------------------------------------------------------------------
globalize γ = γ {renv = globalREnv (renv γ)}

--------------------------------------------------------------------------------
(++=) :: CGEnv -> (String, F.Symbol LHSymbol, SpecType) -> CG CGEnv
--------------------------------------------------------------------------------
(++=) γ (eMsg, x, t)
  = addCGEnv (addRTyConInv (M.unionWith mappend (invs γ) (ial γ))) γ (eMsg, x, t)

--------------------------------------------------------------------------------
addSEnv :: CGEnv -> (String, F.Symbol LHSymbol, SpecType) -> CG CGEnv
--------------------------------------------------------------------------------
addSEnv γ = addCGEnv (addRTyConInv (invs γ)) γ

addEEnv :: CGEnv -> (F.Symbol LHSymbol, SpecType) -> CG CGEnv
addEEnv γ (x,t')= do
  idx   <- fresh
  -- allowHOBinders <- allowHO <$> get
  let t  = addRTyConInv (invs γ) $ normalize idx t'
  let l  = getLocation γ
  let γ' = γ { renv = insertREnv x t (renv γ) }
  tem   <- getTemplates
  is    <- (:) <$> addBind l x (rTypeSortedReft' γ' tem t) <*> addClassBind γ' l t
  modify (\s -> s { ebinds = ebinds s ++ (snd <$> is)})
  return $ γ' { fenv = insertsFEnv (fenv γ) is }


(+++=) :: (CGEnv, String) -> (F.Symbol LHSymbol, CoreExpr, SpecType) -> CG CGEnv
(γ, _) +++= (x, e, t) = (γ {lcb = M.insert x e (lcb γ) }) += ("+++=", x, t)

(-=) :: CGEnv -> F.Symbol LHSymbol -> CGEnv
γ -= x =  γ { renv = deleteREnv x (renv γ)
            , lcb  = M.delete   x (lcb  γ)
            -- , fenv = removeFEnv x (fenv γ)
            }

(?=) :: (?callStack :: CallStack) => CGEnv -> F.Symbol LHSymbol -> Maybe SpecType
γ ?= x  = lookupREnv x (renv γ)

------------------------------------------------------------------------
setLocation :: CGEnv -> Sp.Span -> CGEnv
------------------------------------------------------------------------
setLocation γ p = γ { cgLoc = Sp.push p $ cgLoc γ }

------------------------------------------------------------------------
setBind :: CGEnv -> Var -> CGEnv
------------------------------------------------------------------------
setBind γ x = γ `setLocation` Sp.Var x `setBind'` x

setBind' :: CGEnv -> Tg.TagKey -> CGEnv
setBind' γ k
  | Tg.memTagEnv k (tgEnv γ) = γ { tgKey = Just k }
  | otherwise                = γ

------------------------------------------------------------------------
setRecs :: CGEnv -> [Var] -> CGEnv
------------------------------------------------------------------------
setRecs γ xs   = γ { recs = L.foldl' (flip S.insert) (recs γ) xs }

------------------------------------------------------------------------
setTRec :: CGEnv -> [(Var, SpecType)] -> CGEnv
------------------------------------------------------------------------
setTRec γ xts  = γ' {trec = Just $ M.fromList xts' `M.union` trec'}
  where
    γ'         = γ `setRecs` (fst <$> xts)
    trec'      = fromMaybe M.empty $ trec γ
    -- YL: good Vars
    xts'       = undefined -- first F.symbol <$> xts

------------------------------------------------------------------------
getLocation :: CGEnv -> SrcSpan
------------------------------------------------------------------------
getLocation = Sp.srcSpan . cgLoc
