{-# LANGUAGE RecordWildCards #-}

module Language.Haskell.Liquid.Spec.Aliases (
    -- * Build Refined Type Alias Environment
    makeAliases
  ) where

import Digraph

import Control.Monad

import Data.Maybe

import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S

import Language.Fixpoint.Misc
import Language.Fixpoint.Types hiding (SrcSpan)

import Language.Haskell.Liquid.Types
import Language.Haskell.Liquid.Types.RefType

import qualified Language.Haskell.Liquid.Measure as Ms

import Language.Haskell.Liquid.Spec.Env
import Language.Haskell.Liquid.Spec.Resolve

--------------------------------------------------------------------------------
-- | Build Refined Type Alias Environment---------------------------------------
--------------------------------------------------------------------------------

makeAliases :: Ms.BareSpec -> SpecM RTEnv
makeAliases bspec = do
  let rtEnv0 = mempty
  rtEnv1 <- makeExprAliases rtEnv0 $ Ms.ealiases bspec
  rtEnv2 <- makeTypeAliases rtEnv1 $ Ms.aliases bspec
  return rtEnv2


makeExprAliases :: RTEnv -> [RTAlias Symbol Expr] -> SpecM RTEnv
makeExprAliases = graphExpand findExprEdges addExprAlias

makeTypeAliases :: RTEnv -> [RTAlias Symbol BareType] -> SpecM RTEnv
makeTypeAliases = graphExpand findTypeEdges addTypeAlias

--------------------------------------------------------------------------------
-- | Find Alias Edges ----------------------------------------------------------
--------------------------------------------------------------------------------

findExprEdges :: S.HashSet Symbol -> Expr -> [Symbol]
findExprEdges aliases = go
  where
    go (EVar v)        = go_alias v

    go (EApp e1 e2)    = go e1 ++ go e2
    go (ENeg e)        = go e
    go (EBin _ e1 e2)  = go e1 ++ go e2
    go (EIte _ e1 e2)  = go e1 ++ go e2
    go (ECst e _)      = go e

    go (ESym _)        = []
    go (ECon _)        = []

    go (PAnd ps)       = concatMap go ps
    go (POr ps)        = concatMap go ps
    go (PNot p)        = go p
    go (PImp p q)      = go p ++ go q
    go (PIff p q)      = go p ++ go q
    go (PAll _ p)      = go p
    go (ELam _ e)      = go e

    go (PAtom _ e1 e2) = go e1 ++ go e2

    go (ETApp e _)     = go e
    go (ETAbs e _)     = go e
    go (PKVar _ _)     = []
    go (PExist _ e)    = go e
    go PGrad           = []

    go_alias v         = [v | S.member v aliases]


findTypeEdges :: S.HashSet Symbol -> BareType -> [Symbol]
findTypeEdges aliases = go
  where
    go (RApp c ts rs _)        = concat [ go_alias $ val $ btc_tc c
                                        , concatMap go ts
                                        , concatMap go $ mapMaybe go_ref rs
                                        ]

    go (RFun _ t1 t2 _)        = go t1 ++ go t2
    go (RAppTy t1 t2 _)        = go t1 ++ go t2
    go (RAllE _ t1 t2)         = go t1 ++ go t2
    go (REx _ t1 t2)           = go t1 ++ go t2

    go (RAllT _ t)             = go t
    go (RAllP _ t)             = go t
    go (RAllS _ t)             = go t

    go (RVar _ _)              = []
    go (RExprArg _)            = []
    go (RHole _)               = []

    go (RRTy env _ _ t)        = concatMap (go . snd) env ++ go t

    go_ref (RProp _ (RHole _)) = Nothing
    go_ref (RProp _ t)         = Just t

    go_alias c                 = [c | S.member c aliases]

--------------------------------------------------------------------------------
-- | Add Aliases To Environment ------------------------------------------------
--------------------------------------------------------------------------------

addExprAlias :: RTEnv -> RTAlias Symbol Expr -> SpecM RTEnv
addExprAlias rtEnv alias@RTA{..} = do
  when (length rtTArgs /= 0) $ throwError tyArgsError
  body <- withFixScope rtVArgs $ resolve (rtSrcSpan alias) rtBody
  let alias' = alias { rtBody = body }
  return $ rtEnv { exprAliases = M.insert rtName alias' $ exprAliases rtEnv }
  where
    tyArgsError =
      ErrExprAliasTyArgs (rtSrcSpan alias) (pprint rtName) (pprint <$> rtTArgs)

addTypeAlias :: RTEnv -> RTAlias Symbol BareType -> SpecM RTEnv
addTypeAlias rtEnv alias@RTA{..} = do
  body <- withFixScope rtVArgs $ resolve (rtSrcSpan alias) rtBody
  let alias' = mapRTAVars symbolRTyVar $ alias { rtBody = body }
  return $ rtEnv { typeAliases = M.insert rtName alias' $ typeAliases rtEnv }

--------------------------------------------------------------------------------
-- | Alias Expansion Driver ----------------------------------------------------
--------------------------------------------------------------------------------

type AliasGraph ty = Graph (AliasNode ty)
type AliasNode  ty = Node Symbol (RTAlias Symbol ty)


graphExpand :: (S.HashSet Symbol -> ty -> [Symbol])
            -> (RTEnv -> RTAlias Symbol ty -> SpecM RTEnv)
            -> RTEnv
            -> [RTAlias Symbol ty]
            -> SpecM RTEnv
graphExpand findEdges addAlias rtEnv aliases = do
  let aliasNames = S.fromList $ rtName <$> aliases
  let aliasGraph = buildAliasGraph (findEdges aliasNames) aliases
  checkCyclicAliases aliasGraph
  foldM loop rtEnv $ topSortAliasGraph aliasGraph
  where
    loop rtEnv' = withLocalAliases rtEnv' . addAlias rtEnv'


buildAliasGraph :: (ty -> [Symbol]) -> [RTAlias Symbol ty]
                -> AliasGraph ty
buildAliasGraph findEdges aliases =
  graphFromEdgedVertices $ buildAliasNode findEdges <$> aliases

buildAliasNode :: (ty -> [Symbol])
               -> RTAlias Symbol ty
               -> AliasNode ty
buildAliasNode findEdges alias = (alias, rtName alias, findEdges $ rtBody alias)


checkCyclicAliases :: AliasGraph ty -> SpecM ()
checkCyclicAliases graph =
  case mapMaybe go $ stronglyConnCompG graph of
    [] -> return ()
    cs -> throwErrors $ mkErr <$> cs
  where
    go (AcyclicSCC _) = Nothing
    go (CyclicSCC vs) = findCycle vs

    mkErr cycle = ErrAliasCycle
      { pos    = rtSrcSpan $ safeHead "checkCyclicAliases: empty cycle" cycle
      , acycle = (\rta -> (rtSrcSpan rta, pprint $ rtName rta)) <$> cycle
      }


topSortAliasGraph :: AliasGraph ty
                  -> [RTAlias Symbol ty]
topSortAliasGraph = reverse . (fst3 <$>) . topologicalSortG

