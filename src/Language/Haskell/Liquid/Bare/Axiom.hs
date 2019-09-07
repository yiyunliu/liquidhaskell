{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

-- | This module contains the code that DOES reflection; i.e. converts Haskell
--   definitions into refinements.

-- YL : this could also help understand how to extend liquidhaskell. any resolution going on here?
-- YL : GLOBAL. Anything I can delete with confidence?

module Language.Haskell.Liquid.Bare.Axiom ( makeHaskellAxioms ) where

import Prelude hiding (error)
import Prelude hiding (mapM)
import qualified Control.Exception         as Ex

-- import           Control.Monad.Except      hiding (forM, mapM)
-- import           Control.Monad.State       hiding (forM, mapM)
import qualified Text.PrettyPrint.HughesPJ as PJ -- (text)
import qualified Data.HashSet              as S
import qualified Data.Maybe                as Mb 

import           Language.Fixpoint.Misc
import qualified Language.Haskell.Liquid.Measure as Ms
import qualified Language.Fixpoint.Types as F
import qualified Language.Haskell.Liquid.GHC.API as Ghc 
import           Language.Haskell.Liquid.Types.RefType
import           Language.Haskell.Liquid.Transforms.CoreToLogic
import           Language.Haskell.Liquid.GHC.Misc
import           Language.Haskell.Liquid.Types
import           Language.Haskell.Liquid.Types.LHSymbol

import           Language.Haskell.Liquid.Bare.Resolve as Bare
import           Language.Haskell.Liquid.Bare.Types   as Bare

-----------------------------------------------------------------------------------------------
makeHaskellAxioms :: GhcSrc -> Bare.Env -> Bare.TycEnv -> ModName -> LogicMap -> GhcSpecSig -> Ms.BareSpec 
                  -> [(Ghc.Var, LocSpecType, F.Equation LHSymbol)]
-----------------------------------------------------------------------------------------------
makeHaskellAxioms src env tycEnv name lmap spSig 
  = fmap (makeAxiom env tycEnv name lmap) 
  . getReflectDefs src spSig


getReflectDefs :: GhcSrc -> GhcSpecSig -> Ms.BareSpec 
               -> [(Located F.FixSymbol, Maybe SpecType, Ghc.Var, Ghc.CoreExpr)]
getReflectDefs src sig spec = findVarDefType cbs sigs <$> xs
  where
    sigs                    = gsTySigs sig 
    xs                      = S.toList (Ms.reflects spec)
    cbs                     = giCbs src

findVarDefType :: [Ghc.CoreBind] -> [(Ghc.Var, LocSpecType)] -> Located F.FixSymbol
               -> (Located F.FixSymbol, Maybe SpecType, Ghc.Var, Ghc.CoreExpr)
findVarDefType cbs sigs x = case findVarDef (val x) cbs of
  Just (v, e) -> if Ghc.isExportedId v
                   then (x, val <$> lookup v sigs, v, e)
                   else Ex.throw $ mkError x ("Lifted functions must be exported; please export " ++ show v)
  Nothing     -> Ex.throw $ mkError x "Cannot lift haskell function"

--------------------------------------------------------------------------------
makeAxiom :: Bare.Env -> Bare.TycEnv -> ModName -> LogicMap 
          -> (Located F.FixSymbol, Maybe SpecType, Ghc.Var, Ghc.CoreExpr)
          -> (Ghc.Var, LocSpecType, F.Equation LHSymbol)
--------------------------------------------------------------------------------
makeAxiom env tycEnv name lmap (x, mbT, v, def) 
            = (v, t, e)
  where 
    t       = Bare.qualifyTop env name (F.loc t0) t0 
    (t0, e) = makeAssumeType embs lmap dm x mbT v def
    embs    = Bare.tcEmbs       tycEnv 
    dm      = Bare.tcDataConMap tycEnv 

mkError :: F.Located F.FixSymbol  -> String -> Error
mkError x str = ErrHMeas (sourcePosSrcSpan $ loc x) (pprint $ val x) (PJ.text str)

makeAssumeType
  :: F.TCEmb LHSymbol Ghc.TyCon -> LogicMap -> DataConMap -> F.Located F.FixSymbol -> Maybe SpecType
  -> Ghc.Var -> Ghc.CoreExpr
  -> (LocSpecType, F.Equation LHSymbol)
makeAssumeType tce lmap dm x mbT v def
  -- YL : the equation couldn't contain Var. Now it can..
  = undefined -- (x {val = at `strengthenRes` F.subst su ref},  F.mkEquation (val x) xts le out)
  where
    t     = Mb.fromMaybe (ofType $ Ghc.varType v) mbT
    out   = rTypeSort tce (ty_res tRep)
    at    = F.notracepp ("AXIOM-TYPE: " ++ showpp (x, toType t)) $ axiomType x t
    tRep  = toRTypeRep at
    xArgs = F.EVar <$> [x | (x, t) <- zip (ty_binds tRep) (ty_args tRep), not (isClassType t)]
    _msg  = unwords [showpp x, showpp mbT]
    le    = case runToLogicWithBoolBinds bbs tce lmap dm mkErr (coreToLogic def') of
              Right e -> e
              Left  e -> panic Nothing (show e)
    ref        = F.Reft (F.vv_, F.PAtom F.Eq (F.EVar F.vv_) le)
    mkErr s    = ErrHMeas (sourcePosSrcSpan $ loc x) (pprint $ val x) (PJ.text s)
    bbs        = filter isBoolBind xs
    (xs, def') = grabBody (normalize def)
    su         = undefined -- F.mkSubst  $ zip (F.symbol     <$> xs) xArgs
                 --           ++ zip (simplesymbol <$> xs) xArgs
    xts        = undefined -- zipWith (\x t -> (F.symbol x, rTypeSortExp tce t)) xs ts
    ts         = filter (not . isClassType) (ty_args tRep)

rTypeSortExp :: F.TCEmb LHSymbol Ghc.TyCon -> SpecType -> F.Sort LHSymbol
rTypeSortExp tce = typeSort tce . Ghc.expandTypeSynonyms . toType

grabBody :: Ghc.CoreExpr -> ([Ghc.Var], Ghc.CoreExpr)
grabBody (Ghc.Lam x e)  = (x:xs, e') where (xs, e') = grabBody e
grabBody (Ghc.Tick _ e) = grabBody e
grabBody e              = ([], e)

isBoolBind :: Ghc.Var -> Bool
isBoolBind v = isBool (ty_res $ toRTypeRep ((ofType $ Ghc.varType v) :: RRType ()))

strengthenRes :: SpecType -> F.Reft LHSymbol -> SpecType
strengthenRes t r = fromRTypeRep $ trep {ty_res = ty_res trep `strengthen` F.ofReft r }
  where
    trep = toRTypeRep t


class Subable a where
  subst :: (Ghc.Var, Ghc.CoreExpr) -> a -> a

instance Subable Ghc.Var where
  subst (x, ex) z 
    | x == z, Ghc.Var y <- ex = y
    | otherwise           = z

instance Subable Ghc.CoreExpr where
  subst (x, ex) (Ghc.Var y)
    | x == y    = ex
    | otherwise = Ghc.Var y
  subst su (Ghc.App f e)
    = Ghc.App (subst su f) (subst su e)
  subst su (Ghc.Lam x e)
    = Ghc.Lam x (subst su e)
  subst su (Ghc.Case e x t alts)
    = Ghc.Case (subst su e) x t (subst su <$> alts)
  subst su (Ghc.Let (Ghc.Rec xes) e)
    = Ghc.Let (Ghc.Rec (mapSnd (subst su) <$> xes)) (subst su e)
  subst su (Ghc.Let (Ghc.NonRec x ex) e)
    = Ghc.Let (Ghc.NonRec x (subst su ex)) (subst su e)
  subst su (Ghc.Cast e t)
    = Ghc.Cast (subst su e) t
  subst su (Ghc.Tick t e)
    = Ghc.Tick t (subst su e)
  subst _ e 
    = e 

instance Subable Ghc.CoreAlt where
  subst su (c, xs, e) = (c, xs, subst su e)

-- | Specification for Haskell function
axiomType :: (TyConable c) => F.Located F.FixSymbol  -> RType c tv RReft -> RType c tv RReft
axiomType s t = undefined -- fromRTypeRep (tr {ty_res = res, ty_binds = xs})
  -- where
  --   res           = strengthen (ty_res tr) (singletonApp s ys)
  --   ys            = fst $ unzip $ dropWhile (isClassType . snd) xts
  --   xts           = safeZip "axiomBinds" xs (ty_args tr)
  --   xs            = zipWith unDummy bs [1..]
  --   tr            = toRTypeRep t
  --   bs            = ty_binds tr

unDummy :: F.FixSymbol -> Int -> F.FixSymbol
unDummy x i
  | x /= F.dummySymbol = x
  | otherwise          = F.symbol ("lq" ++ show i)

singletonApp :: F.FixSymbolic a => Located F.FixSymbol -> [a] -> UReft (F.Reft LHSymbol)
singletonApp s ys = MkUReft r mempty mempty
  where
    r             = undefined -- uF.exprReft (F.mkEApp s (F.eVar <$> ys))
