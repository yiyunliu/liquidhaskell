module Language.Haskell.Liquid.Types.Bounds (
    RRBEnv
  , RRBound
  , Bound
  ) where

import qualified Data.HashMap.Strict as M

import Language.Fixpoint.Types

type RRBEnv tv     = M.HashMap LocSymbol (RRBound tv)
type RRBound tv    = Bound tv Expr

data Bound t e

