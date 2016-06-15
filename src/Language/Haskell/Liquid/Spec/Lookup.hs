{-# LANGUAGE OverloadedStrings #-}

module Language.Haskell.Liquid.Spec.Lookup (
    -- * Querying the GHC Context
    lookupGhcVar
  , lookupGhcVarL
  , lookupGhcTyCon
  , lookupGhcTyConL
  , lookupGhcDataCon
  , lookupGhcDataConL
  ) where

import GHC

import Bag
import BasicTypes
import HscTypes
import MonadUtils
import TysWiredIn
import Var

import Data.List
import Data.Maybe

import Text.PrettyPrint.HughesPJ (text)

import Language.Fixpoint.Types hiding (Error, SrcSpan)

import Language.Haskell.Liquid.GHC.Misc
import Language.Haskell.Liquid.Types
import Language.Haskell.Liquid.WiredIn

import Language.Haskell.Liquid.Spec.Env

--------------------------------------------------------------------------------
-- | Querying the GHC Context --------------------------------------------------
--------------------------------------------------------------------------------

lookupGhcVar :: SrcSpan -> Symbol -> SpecM Var
lookupGhcVar = lookupGhcThing "variable or data constructor" tyThingId_maybe

lookupGhcVarL :: LocSymbol -> SpecM Var
lookupGhcVarL x = lookupGhcVar (fSrcSpan x) (val x)

lookupGhcTyCon :: SrcSpan -> Symbol -> SpecM TyCon
lookupGhcTyCon l x =
  lookupGhcThing "type constructor or class" tyThingTyCon_maybe l x
    `catchError` tryPropTyCon x

lookupGhcTyConL :: LocSymbol -> SpecM TyCon
lookupGhcTyConL x = lookupGhcTyCon (fSrcSpan x) (val x)

lookupGhcDataCon :: SrcSpan -> Symbol -> SpecM DataCon
lookupGhcDataCon l x
  | "(," `isPrefixOfSym` x =
    return $ tupleCon BoxedTuple $ lengthSym x - 1
  | x == "[]" =
    return nilDataCon
  | x == ":" =
    return consDataCon
  | otherwise =
    lookupGhcThing "data constructor" tyThingDataCon_maybe l x

lookupGhcDataConL :: LocSymbol -> SpecM DataCon
lookupGhcDataConL x = lookupGhcDataCon (fSrcSpan x) (val x)

lookupGhcThing :: String -> (TyThing -> Maybe a) -> SrcSpan -> Symbol -> SpecM a
lookupGhcThing desc unwrap l x = handleSourceError handle $ do
  names <- parseName $ symbolString x
  things <- mapMaybeM lookupName names
  let thing = listToMaybe $ mapMaybe unwrap things
  case thing of
    Just x -> return x
    Nothing -> throwError notInScopeError
  where
    handle = throwErrors . map ghcError . bagToList . srcErrorMessages

    notInScopeError = ErrNotInScope l (text desc) (pprint x)
    ghcError e
      | "Not in scope:"  `isPrefixOf` show e = notInScopeError
      | otherwise = ErrGhc l $ pprint e


tryPropTyCon :: Symbol -> Error -> SpecM TyCon
tryPropTyCon x e
  | x == propConName  = return propTyCon
  | x == hpropConName = return hpropTyCon
  | otherwise         = throwError e

