{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module Language.Haskell.Liquid.Types.Variance (
    Variance(..)
  , VarianceInfo
  , VarianceEnv
  ) where

import Prelude hiding (error)

import TyCon

import Control.DeepSeq

import Data.Data hiding (TyCon)

import qualified Data.HashMap.Strict as M

import GHC.Generics

import Text.PrettyPrint.HughesPJ

import Language.Fixpoint.Types

type VarianceEnv  = M.HashMap TyCon (Located VarianceInfo)
type VarianceInfo = [Variance]

data Variance = Invariant | Bivariant | Contravariant | Covariant
              deriving (Eq, Enum, Data, Typeable, Show, Generic)

instance NFData Variance

instance PPrint Variance where
  pprintTidy _ = text . show

