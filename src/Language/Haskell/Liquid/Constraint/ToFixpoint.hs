module Language.Haskell.Liquid.Constraint.ToFixpoint (

  targetFInfo

  ) where

import           Prelude hiding (error)
import           Data.Bifunctor
import           Data.List
import qualified Data.HashMap.Strict as M
import qualified Language.Fixpoint.Types        as F
import           Language.Haskell.Liquid.Constraint.Types
import           Language.Haskell.Liquid.Types hiding     ( binds )
import           Language.Haskell.Liquid.Constraint.Qualifier

targetFInfo :: GhcInfo -> CGInfo -> FilePath -> F.FInfo Cinfo
targetFInfo info cgi fn = F.fi cs ws bs ls ks packs qs bi fn aHO aHOqs 
  where
   packs = F.makePack (kvPacks cgi)
   cs    = fixCs  cgi
   ws    = fixWfs cgi
   bs    = binds  cgi
   ls    = foldl' (flip (uncurry F.insertSEnv . first val)) (fEnv cgi) $
             M.toList $ constants $ gblSpec info
   ks    = kuts cgi
   qs    = targetQuals info cgi
   bi    = (`Ci` Nothing) <$> bindSpans cgi
   aHO   = allowHO cgi 
   aHOqs = higherorderqs $ config info 

targetQuals :: GhcInfo -> CGInfo -> [F.Qualifier]
targetQuals info cgi = spcQs ++ genQs
  where
    spcQs     = M.elems $ qualifiers $ gblSpec info
    genQs     = specificationQualifiers n info (fEnv cgi)
    n         = maxParams $ config info
    -- lEnv      = F.fromListSEnv $ lits cgi
