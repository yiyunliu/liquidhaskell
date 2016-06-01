{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Haskell.Liquid.Spec.Resolve (
    -- * Resolve Names in Symbols
    Resolve
  , resolve
  , resolveL
  ) where

import GHC hiding (Located)

import BasicTypes
import Name
import Type
import TysWiredIn

import Control.Monad
import Control.Monad.Reader

import qualified Data.HashMap.Strict as M

import Language.Fixpoint.Types hiding (Predicate, SrcSpan)

import Language.Haskell.Liquid.GHC.Misc hiding (synTyConRhs_maybe)
import Language.Haskell.Liquid.Misc
import Language.Haskell.Liquid.Types
import Language.Haskell.Liquid.Types.Bounds
import Language.Haskell.Liquid.Types.RefType

import Language.Haskell.Liquid.Spec.Env
import Language.Haskell.Liquid.Spec.Lookup

--------------------------------------------------------------------------------
-- | Resolve Names in Symbols --------------------------------------------------
--------------------------------------------------------------------------------

resolve :: Resolve a b => SrcSpan -> a -> SpecM b
resolve l x = runResolveM (resolve' x) mempty l

resolveL :: Resolve a b => Located a -> SpecM (Located b)
resolveL x = traverse (resolve $ fSrcSpan x) x

--------------------------------------------------------------------------------
-- | Internal Resolution Monad -------------------------------------------------
--------------------------------------------------------------------------------

type ResolveM = ReaderT ResolveEnv SpecM

data ResolveEnv
  = ResolveEnv
    { resolveEnvPredVars :: !(M.HashMap Symbol UsedPVar)
    , resolveEnvLocation :: !SrcSpan
    }

runResolveM :: ResolveM a
            -> M.HashMap Symbol UsedPVar
            -> SrcSpan
            -> SpecM a
runResolveM act pvs l = runReaderT act $ ResolveEnv
  { resolveEnvPredVars = pvs
  , resolveEnvLocation = l
  }


withPredVar :: UsedPVar -> ResolveM a -> ResolveM a
withPredVar pv act = withReaderT adj act
  where
    adj env = env
      { resolveEnvPredVars = M.insert (pname pv) pv $ resolveEnvPredVars env
      }

lookupPredVar :: Symbol -> ResolveM (Maybe UsedPVar)
lookupPredVar s = asks $ M.lookup s . resolveEnvPredVars


withLocation :: SrcSpan -> ResolveM a -> ResolveM a
withLocation l act = withReaderT adj act
  where
    adj env = env { resolveEnvLocation = l }

getLocation :: ResolveM SrcSpan
getLocation = asks resolveEnvLocation

--------------------------------------------------------------------------------
-- | Resolution Traversal ------------------------------------------------------
--------------------------------------------------------------------------------

class Resolve a b | a -> b where
  resolve' :: a -> ResolveM b

instance Resolve () () where
  resolve' = return

instance Resolve a b => Resolve [a] [b] where
  resolve' = mapM resolve'

instance Resolve a b => Resolve (Located a) (Located b) where
  resolve' x = withLocation (fSrcSpan x) $ traverse resolve' x

-- TODO: Handle type variables properly (symbolRTyVar is probably wrong)
instance ( PPrint r
         , Resolve r r
         , SubsTy Symbol BSort r
         , SubsTy RTyVar RSort r
         , UReftable r
         ) => Resolve (BRType r) (RRType r) where
  resolve' (RVar v r) =
    RVar (symbolRTyVar v) <$> resolve' r

  resolve' (RFun b i o r) =
    resolveRFun b i o r

  resolve' (RAllT tv ty) =
    RAllT (symbolRTyVar tv) <$> resolve' ty
  resolve' (RAllP pv ty) = do
    pv' <- resolve' pv
    RAllP pv' <$> withPredVar (uPVar pv') (resolve' ty)
  resolve' (RAllS sb ty) =
    RAllS sb <$> withFixScope [sb] (resolve' ty)

  resolve' (RApp tc ts ps r) =
    resolveRApp tc ts ps r

  -- TOOD: Correct scoping behavior?
  resolve' (RAllE b a ty) =
    RAllE b <$> resolve' a <*> withFixScope [b] (resolve' ty)
  -- TOOD: Correct scoping behavior?
  resolve' (REx b e ty) =
    REx b <$> resolve' e <*> withFixScope [b] (resolve' ty)

  resolve' (RExprArg e) =
    RExprArg <$> resolve' e

  resolve' (RAppTy t1 t2 r) =
    RAppTy <$> resolve' t1 <*> resolve' t2 <*> resolve' r

  -- TODO: Correct scoping behavior?
  resolve' (RRTy env ref obl ty) =
    RRTy <$> mapM (secondM resolve') env
         <*> resolve' ref
         <*> pure obl
         <*> withFixScope (fst <$> env) (resolve' ty)

  resolve' (RHole r) =
    RHole <$> resolve' r

instance (Resolve a1 b1, Resolve a2 b2) => Resolve (Ref a1 a2) (Ref b1 b2) where
  -- TODO: Correct scoping behavior?
  resolve' (RProp as b) =
    RProp <$> mapM (secondM resolve') as
          <*> withFixScope (fst <$> as) (resolve' b)

instance Resolve a b => Resolve (PVar a) (PVar b) where
  resolve' (PV n k a as) =
    PV n <$> resolve' k
         <*> pure a
         <*> mapM (\(t,s,e) -> (,s,) <$> resolve' t <*> resolve' e) as

instance Resolve a b => Resolve (PVKind a) (PVKind b) where
  resolve' (PVProp t) =
    PVProp <$> resolve' t
  resolve' PVHProp =
    return PVHProp

instance Resolve a b => Resolve (UReft a) (UReft b) where
  resolve' (MkUReft r p s) =
    MkUReft <$> resolve' r <*> resolve' p <*> return s

instance Resolve Reft Reft where
  resolve' (Reft (s, ra)) =
    Reft . (s,) <$> withFixScope [s] (resolve' ra)

instance Resolve Predicate Predicate where
  resolve' (Pr pvs) =
    Pr <$> (mapM replaceParam =<< resolve' pvs)

instance Resolve Qualifier Qualifier where
  -- TODO: Correct scoping behavior?
  resolve' (Q n ps b l) = withLocation (sourcePosSrcSpan l) $
    Q n <$> mapM (secondM resolve') ps
        <*> withFixScope (fst <$> ps) (resolve' b)
        <*> return l

instance Resolve Expr Expr where
  resolve' (EVar f) =
    resolveExprApp f []
  resolve' e@EApp{} =
    case splitEApp e of
      (EVar f, es) -> resolveExprApp f =<< resolve' es
      (f, es) -> eApps <$> resolve' f <*> resolve' es

  resolve' (ENeg e) =
    ENeg <$> resolve' e
  resolve' (EBin op e1 e2) =
    EBin op <$> resolve' e1 <*> resolve' e2
  resolve' (EIte p e1 e2) =
    EIte <$> resolve' p <*> resolve' e1 <*> resolve' e2
  resolve' (ECst x s) =
    ECst <$> resolve' x <*> resolve' s

  resolve' e@ESym{} =
    return e
  resolve' e@ECon{} =
    return e

  resolve' (PAnd ps) =
    PAnd <$> resolve' ps
  resolve' (POr ps) =
    POr <$> resolve' ps
  resolve' (PNot p) =
    PNot <$> resolve' p
  resolve' (PImp p q) =
    PImp <$> resolve' p <*> resolve' q
  resolve' (PIff p q) =
    PIff <$> resolve' p <*> resolve' q
  resolve' (PAll vs p) =
    PAll <$> mapM (secondM resolve') vs <*> resolve' p
  resolve' (ELam (x, t) e) =
    ELam <$> ((x,) <$> withFixScope [x] (resolve' t)) <*> resolve' e

  resolve' (ETApp e s) =
    ETApp <$> resolve' e <*> resolve' s
  resolve' (ETAbs e _s) =
    ETAbs <$> resolve' e <*> error "FIXME: What is ETAbs's Symbol?"

  resolve' (PAtom b e1 e2) =
    PAtom b <$> resolve' e1 <*> resolve' e2

  resolve' e@PKVar{} =
    return e

  resolve' PGrad =
    return PGrad

  resolve' (PExist s e) =
    PExist s <$> resolve' e

instance Resolve Sort Sort where
  resolve' FInt =
    return FInt
  resolve' FReal =
    return FReal
  resolve' FNum =
    return FNum
  resolve' FFrac =
    return FFrac
  resolve' s@FObj{} =
    return s
  resolve' s@FVar{} =
    return s
  resolve' (FAbs i s) =
    FAbs i <$> resolve' s
  resolve' (FFunc s1 s2) =
    FFunc <$> resolve' s1 <*> resolve' s2
  resolve' (FTC c) =
    FTC <$> resolveTyCon c
  resolve' (FApp t1 t2) =
    FApp <$> resolve' t1 <*> resolve' t2

--------------------------------------------------------------------------------
-- | Resolution and Expansion in Refined Types ---------------------------------
--------------------------------------------------------------------------------

resolveRFun :: ( PPrint r
--             , Resolve (BRType r) (RRType r)
               , Resolve r r
               , SubsTy Symbol BSort r
               , SubsTy RTyVar RSort r
               , UReftable r
               )
            => Symbol -> BRType r -> BRType r -> r -> ResolveM (RRType r)
resolveRFun b i o r = go_bound
  where
    -- TOOD: Error on wrong arity & non-tauto refinements
    go_bound = case i of
      RApp c ps _ _ -> do
        result <- lookupBound $ val c
        case result of
          Just bound  -> do
            let (ts, ps') = splitAt (length $ tyvars bound) ps
            ts' <- resolve' ts
            makeBound bound ts' [x | RVar x _ <- ps'] <$> resolve' o
          Nothing -> go_fun
      _ -> go_fun
    go_fun = RFun b <$> resolve' i <*> resolve' o <*> resolve' r

resolveRApp :: ( PPrint r
--             , Resolve (BRType r) (RRType r)
--             , Resolve (RTProp LocSymbol Symbol r) (RTProp RTycon RTyVar r)
               , Resolve r r
               , SubsTy Symbol BSort r
               , SubsTy RTyVar RSort r
               , UReftable r
               )
            => LocSymbol -> [BRType r] -> [RTProp LocSymbol Symbol r] -> r
            -> ResolveM (RRType r)
resolveRApp c ts ps r = do
  result <- lookupTypeAlias $ val c
  case result of
    Just rta -> expandTypeAlias (fSrcSpan c) rta ts ps r
    Nothing  -> appSpecType c ts ps r

expandTypeAlias :: ( PPrint r
--                 , Resolve (BRType r) (RRType r)
                   , Resolve r r
                   , SubsTy Symbol BSort r
                   , SubsTy RTyVar RSort r
                   , UReftable r)
                => SrcSpan
                -> RTAlias RTyVar SpecType
                -> [BRType r]
                -> [RTProp LocSymbol Symbol r]
                -> r
                -> ResolveM (RRType r)
expandTypeAlias l RTA{..} ts ps r = do
  unless (null ps) $ throwError predArgsError
  unless (length ts == length rtTArgs + length rtVArgs) $ throwError appError
  targs <- resolve' $ take (length rtTArgs) ts
  vargs <- mapM (resolve' <=< toExprArg) $ drop (length rtTArgs) ts
  let tsu = zipWith (\a t -> (a, toRSort t, t)) rtTArgs targs
  let vsu = mkSubst $ zip (symbol <$> rtVArgs) vargs
  r' <- resolve' r
  let body = mapReft ofUReft rtBody
  return $ subst vsu $ (`strengthen` r') $ subsTyVars_meet tsu body
  where
    predArgsError =
      ErrTypeAliasPredArgs l $ pprint rtName
    appError =
      ErrTypeAliasApp l (length ts) (pprint rtName)
                        (sourcePos2SrcSpan rtPos rtPosE)
                        (length rtTArgs) (length rtVArgs)

-- | toExprArg converts a tyVar to an exprVar because parser cannot tell
-- HORRIBLE HACK To allow treating upperCase X as value variables X
-- e.g. type Matrix a Row Col = List (List a Row) Col
toExprArg :: ( PPrint r
             , Reftable r
             , SubsTy Symbol BSort r
             )
          => BRType r -> ResolveM Expr
toExprArg (RExprArg e) =
  return $ val e
toExprArg (RVar x r) | isTauto r =
  return $ EVar $ symbol x
toExprArg (RApp x [] [] r) | isTauto r =
  return $ EVar $ symbol x
toExprArg (RApp f ts [] r) | isTauto r =
  mkEApp (symbol <$> f) <$> mapM toExprArg ts
toExprArg (RAppTy (RVar f r1) t r2) | isTauto r1 && isTauto r2 =
  mkEApp (dummyLoc $ symbol f) . return <$> toExprArg t
toExprArg z =
  throwError . (`ErrNotAnExprArg` pprint z) =<< getLocation

appSpecType :: ( PPrint r
--             , Resolve (BRType r) (RRType r)
--             , Resolve (RTProp LocSymbol Symbol r) (RTProp RTyCon RTyVar r)
               , Resolve r r
               , SubsTy Symbol BSort r
               , SubsTy RTyVar RSort r
               , UReftable r
               )
            => LocSymbol -> [BRType r] -> [RTProp LocSymbol Symbol r] -> r
            -> ResolveM (RRType r)
appSpecType c ts ps r = do
  c'  <- matchTyCon c $ length ts
  ts' <- resolve' ts
  ps' <- resolve' ps
  r'  <- resolve' r
  appSpecType' (fSrcSpan c) c' ts' ps' r'

-- TODO: Better type application

appSpecType' :: ( PPrint r
                , SubsTy Symbol BSort r
                , SubsTy RTyVar RSort r
                , UReftable r
                )
             => SrcSpan
             -> TyCon
             -> [RRType r]
             -> [RTProp RTyCon RTyVar r]
             -> r
             -> ResolveM (RRType r)

appSpecType' l c ts ps r | Just rhs <- synTyConRhs_maybe c = do
  when (realTcArity c < length ts) $ throwError arityError
  case subsTyVars_meet su $ ofType rhs of
    -- TODO: RAppTy?
    RApp c' ts' ps' r' ->
      return $ RApp c' (ts' ++ as) (ps' ++ ps) (r' `meet` r)
    t | null as && null ps ->
      return $ t `strengthen` r
      | otherwise ->
      throwError $ badAppError t
  where
    tvs = tyConTyVarsDef c
    su  = zipWith (\a t -> (rTyVar a, toRSort t, t)) tvs ts
    as  = drop (length tvs) ts

    arityError =
      ErrTypeAliasApp l (length ts) (pprint c) (getSrcSpan c) (realTcArity c) 0
    badAppError t =
      ErrBadTypeApp l (mapReft toUReft t) (mapReft toUReft <$> ts)

appSpecType' _ c ts ps r | isFamilyTyCon c && isTrivial t =
  return $ (ofType $ expandTypeSynonyms $ toType t) `strengthen` r
  where
    t = rApp c ts ps mempty

appSpecType' _ c ts ps r =
  return $ rApp c ts ps r

matchTyCon :: LocSymbol -> Int -> ResolveM TyCon
matchTyCon lc@(Loc _ _ c) arity
  | isList c = return listTyCon
  | isTuple c = return $ tupleTyCon BoxedTuple arity
  | otherwise = lift $ lookupGhcTyConL lc

replaceParam :: UsedPVar -> ResolveM UsedPVar
replaceParam pv = do
  result <- lookupPredVar $ pname pv
  case result of
    Nothing  -> throwError . unboundError =<< getLocation
    Just pv' -> return $ pv { pargs = mergeArgs (pargs pv') (pargs pv) }
  where
    mergeArgs xs [] = xs
    mergeArgs xs ys = zipWith (\(_, x, _) (t, _, y) -> (t, x, y)) xs ys

    unboundError l = ErrUnboundPVar l $ pprint pv

--------------------------------------------------------------------------------
-- | Resolution and Expansion in Expressions -----------------------------------
--------------------------------------------------------------------------------

resolveTyCon :: FTycon -> ResolveM FTycon
resolveTyCon c
  | tcs' `elem` prims = return c
  | otherwise = symbolFTycon . Loc l l' . symbol <$> lift (lookupGhcTyConL tcs)
  where
    tcs@(Loc l l' tcs') = fTyconSymbol c

resolveExprApp :: Symbol -> [Expr] -> ResolveM Expr
resolveExprApp f es = go_scope
  where
    go_scope = do
      inScope <- isInFixScope f
      if inScope
        then go_app f
        else go_alias

    go_alias = do
      result <- lookupExprAlias f
      case result of
        Just rta -> expandExprApp rta es
        Nothing  -> go_prims

    go_prims
      | f `elem` prims = go_app f
      | otherwise      = go_lookup

    go_lookup = do
      l <- getLocation
      v <- lift $ lookupGhcVar l f
      go_app $ varSymbol v

    go_app f' = return $ eApps (EVar f') es

expandExprApp :: RTAlias Symbol Expr -> [Expr] -> ResolveM Expr
expandExprApp rta es
  | nargs == dargs =
    return $ subst (mkSubst $ zip (rtVArgs rta) es) $ rtBody rta
  | otherwise =
    throwError . mkErr =<< getLocation
  where
    mkErr l = ErrExprAliasApp l nargs dname dpos dargs

    nargs = length es
    dname = pprint $ rtName rta
    dpos  = sourcePos2SrcSpan (rtPos rta) (rtPosE rta)
    dargs = length $ rtVArgs rta

