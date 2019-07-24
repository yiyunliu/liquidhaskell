{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module Language.Haskell.Liquid.Types.Bounds (

    Bound(..),

    RBound, RRBound,

    RBEnv, RRBEnv,

    makeBound,

    ) where

import Prelude hiding (error)
import Text.PrettyPrint.HughesPJ
import GHC.Generics
import Data.List (partition)
import Data.Maybe
import Data.Hashable
import Data.Bifunctor
import Data.Data
import qualified Data.Binary         as B
import qualified Data.HashMap.Strict as M

import qualified Language.Fixpoint.Types as F
import qualified Language.Fixpoint.Misc  as Misc -- (mapFst, mapSnd)
import Language.Haskell.Liquid.Types.Types
import Language.Haskell.Liquid.Types.LHSymbol
import Language.Haskell.Liquid.Types.RefType


data Bound t e = Bound
  { bname   :: Located F.FixSymbol         -- ^ The name of the bound
  , tyvars  :: [t]               -- ^ Type variables that appear in the bounds
  , bparams :: [(Located F.FixSymbol, t)]  -- ^ These are abstract refinements, for now
  , bargs   :: [(Located F.FixSymbol, t)]  -- ^ These are value variables
  , bbody   :: e                 -- ^ The body of the bound
  } deriving (Data, Typeable, Generic)

instance (B.Binary t, B.Binary e) => B.Binary (Bound t e)

type RBound        = RRBound RSort
type RRBound tv    = Bound tv (F.Expr LHSymbol)
type RBEnv         = M.HashMap (Located F.FixSymbol) RBound
type RRBEnv tv     = M.HashMap (Located F.FixSymbol) (RRBound tv)


instance Hashable (Bound t e) where
  hashWithSalt i = hashWithSalt i . bname

instance Eq (Bound t e) where
  b1 == b2 = bname b1 == bname b2

instance (PPrint e, PPrint t) => (Show (Bound t e)) where
  show = showpp


instance (PPrint e, PPrint t) => (PPrint (Bound t e)) where
  pprintTidy k (Bound s vs ps xs e) = "bound" <+> pprintTidy k s <+>
                                      "forall" <+> pprintTidy k vs <+> "." <+>
                                      pprintTidy k (fst <$> ps) <+> "=" <+>
                                      ppBsyms k (fst <$> xs) <+> pprintTidy k e
    where
      ppBsyms _ [] = ""
      ppBsyms k xs = "\\" <+> pprintTidy k xs <+> "->"

instance Bifunctor Bound where
  first  f (Bound s vs ps xs e) = Bound s (f <$> vs) (Misc.mapSnd f <$> ps) (Misc.mapSnd f <$> xs) e
  second f (Bound s vs ps xs e) = Bound s vs ps xs (f e)

makeBound :: (PPrint r, UReftable r, SubsTy RTyVar (RType RTyCon RTyVar ()) r)
          => RRBound RSort -> [RRType r] -> [F.FixSymbol] -> RRType r -> RRType r
makeBound (Bound _  vs ps xs p) ts qs
         = RRTy (Misc.mapFst (F.AS . LHRefSym) <$> cts) mempty OCons
  where
    cts  = (\(x, t) -> (x, foldr subsTyVar_meet t su)) <$> cts'

    cts' = makeBoundType penv rs xs

    penv = zip (val . fst <$> ps) qs
    rs   = bkImp [] p

    bkImp acc (F.PImp p q) = bkImp (p:acc) q
    bkImp acc p          = p:acc

    su  = [(α, toRSort t, t) | (RVar α _, t) <-  zip vs ts ]

makeBoundType :: (PPrint r, UReftable r)
              => [(F.FixSymbol, F.FixSymbol)]
              -> [F.Expr LHSymbol]
              -> [(Located F.FixSymbol, RSort)]
              -> [(F.FixSymbol, RRType r)]
makeBoundType penv (q:qs) xts = go xts
  where
    -- NV TODO: Turn this into a proper error
    go [] = panic Nothing "Bound with empty symbols"

    go [(x, t)]      = [(F.dummySymbol, tp t x), (F.dummySymbol, tq t x)]
    go ((x, t):xtss) = (val x, mkt t x) : go xtss

    mkt t x = ofRSort t `strengthen` ofUReft (MkUReft (F.Reft (val x, mempty))
                                                (Pr $ M.lookupDefault [] (val x) ps) mempty)
    tp t x  = ofRSort t `strengthen` ofUReft (MkUReft (F.Reft (val x, F.pAnd rs))
                                                (Pr $ M.lookupDefault [] (val x) ps) mempty)
    tq t x  = ofRSort t `strengthen` makeRef penv x q

    (ps, rs) = partitionPs penv qs


-- NV TODO: Turn this into a proper error
makeBoundType _ _ _           = panic Nothing "Bound with empty predicates"


partitionPs :: [(F.FixSymbol, F.FixSymbol)] -> [F.Expr LHSymbol] -> (M.HashMap F.FixSymbol [UsedPVar], [F.Expr LHSymbol])
partitionPs penv qs = Misc.mapFst makeAR $ partition (isPApp penv) qs
  where
    makeAR ps       = M.fromListWith (++) $ map (toUsedPVars penv) ps

isPApp :: [(F.FixSymbol, a)] -> F.Expr LHSymbol -> Bool
isPApp penv (F.EApp (F.EVar p) _)  = isJust $ lookup p penv
isPApp penv (F.EApp e _)         = isPApp penv e
isPApp _    _                  = False

toUsedPVars :: [(F.FixSymbol, F.FixSymbol)] -> F.Expr LHSymbol -> (F.FixSymbol, [PVar ()])
toUsedPVars penv q@(F.EApp _ e) = (x, [toUsedPVar penv q])
  where
    -- NV : TODO make this a better error
    x = case {- unProp -} e of {F.EVar x -> x; e -> todo Nothing ("Bound fails in " ++ show e) }
toUsedPVars _ _ = impossible Nothing "This cannot happen"

toUsedPVar :: [(F.FixSymbol, F.FixSymbol)] -> F.Expr LHSymbol -> PVar ()
toUsedPVar penv ee@(F.EApp _ _)
  = PV q (PVProp ()) e (((), F.dummySymbol,) <$> es')
   where
     F.EVar e = {- unProp $ -} last es
     es'    = init es
     Just q = lookup p penv
     (F.EVar p, es) = F.splitEApp ee

toUsedPVar _ _ = impossible Nothing "This cannot happen"

-- `makeRef` is used to make the refinement of the last implication,
-- thus it can contain both concrete and abstract refinements

makeRef :: (UReftable r) => [(F.FixSymbol, F.FixSymbol)] -> Located F.FixSymbol -> F.Expr LHSymbol -> r
makeRef penv v (F.PAnd rs) = ofUReft (MkUReft (F.Reft (F.AS . LHRefSym . val $ v, F.pAnd rrs)) r mempty)
  where
    r                    = Pr  (toUsedPVar penv <$> pps)
    (pps, rrs)           = partition (isPApp penv) rs

makeRef penv v rr
  | isPApp penv rr       = ofUReft (MkUReft (F.Reft(F.AS . LHRefSym . val $ v, mempty)) r mempty)
  where
    r                    = Pr [toUsedPVar penv rr]

makeRef _    v p         = F.ofReft (F.Reft (F.AS . LHRefSym . val $ v, p))
