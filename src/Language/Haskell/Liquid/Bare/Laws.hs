-- YL : it would at least be useful to learn how this file works


module Language.Haskell.Liquid.Bare.Laws ( makeInstanceLaws ) where

import qualified Data.Maybe                                 as Mb
import qualified Data.List                                  as L
import qualified Data.HashMap.Strict                        as M
import           Control.Monad (join)

import qualified Language.Haskell.Liquid.Measure            as Ms
import qualified Language.Fixpoint.Types                    as F
import qualified Language.Haskell.Liquid.GHC.Misc           as GM
import           Language.Haskell.Liquid.Bare.Types         as Bare 
import           Language.Haskell.Liquid.Bare.Resolve       as Bare
import           Language.Haskell.Liquid.Bare.Expand        as Bare
import           Language.Haskell.Liquid.Types
import           Language.Haskell.Liquid.GHC.API            



makeInstanceLaws :: Bare.Env -> Bare.SigEnv -> [(Var,LocSpecType)] 
                -> Bare.ModSpecs -> [LawInstance]
makeInstanceLaws env sigEnv sigs specs 
  = [makeInstanceLaw env sigEnv sigs name rilaw 
              | (name, spec) <- M.toList specs
              , rilaw <- Ms.ilaws spec ]


makeInstanceLaw :: Bare.Env -> Bare.SigEnv -> [(Var,LocSpecType)] -> ModName  
                -> RILaws LocBareType -> LawInstance
makeInstanceLaw env sigEnv sigs name rilaw = LawInstance
  { lilName   = Mb.fromMaybe errmsg tc
  , liSupers  = mkTy <$> rilSupers rilaw 
  , lilTyArgs = mkTy <$> rilTyArgs rilaw
  , lilEqus   = undefined -- [(mkVar l, mkTypedVar r) | (l,r)<- rilEqus rilaw ]  
  , lilPos    = GM.sourcePosSrcSpan $ loc $ rilPos rilaw
  } 
  where
    tc    :: Maybe Class 
    tc     = classTc (rilName rilaw)
    errmsg = error ("Not a type class: " ++ F.showpp tc)

    classTc = join . fmap tyConClass_maybe . Bare.maybeResolveSym env name "makeClass" . btc_tc  

    mkTy :: LocBareType -> LocSpecType
    mkTy = Bare.cookSpecType env sigEnv name Bare.GenTV 
    mkVar :: F.Located F.FixSymbol -> VarOrLocSymbol 
    mkVar x = case Bare.maybeResolveSym env name "makeInstanceLaw" x of 
                Just v -> Left v 
                _      -> Right x 

    mkTypedVar :: F.Located F.FixSymbol -> (VarOrLocSymbol, Maybe LocSpecType)
    mkTypedVar l = case mkVar l of 
                     Left x -> (Left x, Just $ Mb.fromMaybe (dummyLoc $ ofType $ varType x) (L.lookup x sigs))
                     Right x -> (Right x, Nothing)

