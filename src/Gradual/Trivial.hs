module Gradual.Trivial (simplify) where

import Language.Fixpoint.Types  hiding (simplify)
import qualified Data.HashMap.Strict       as M
import Language.Haskell.Liquid.Types.LHSymbol

simplify :: SInfo LHSymbol a -> SInfo LHSymbol a 
simplify sinfo = sinfo {cm = M.map f (cm sinfo)}
  where
    f c | PGrad _ _ _ e <- _crhs c 
      = c { _crhs = e} 
    f c = c 
