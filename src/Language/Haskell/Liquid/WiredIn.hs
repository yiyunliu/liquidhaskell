{-# LANGUAGE OverloadedStrings #-}

module Language.Haskell.Liquid.WiredIn
       ( wiredTyCons
       , wiredDataCons
       , wiredSortedSyms

       -- * Constants for automatic proofs
       , dictionaryVar
       , dictionaryTyVar
       , dictionaryBind
       , proofTyConName
       , combineProofsName

       -- * Built in symbols
       , isWiredIn
       , isWiredInName
       , dcPrefix

       -- * Deriving classes 
       , isDerivedInstance 
       ) where

import Prelude                                hiding (error)
import Var

-- import Language.Fixpoint.Misc           (mapSnd)
import Language.Haskell.Liquid.GHC.Misc
import qualified Language.Haskell.Liquid.GHC.API as Ghc
import Language.Haskell.Liquid.Types.Types
import Language.Haskell.Liquid.Types.RefType
import Language.Haskell.Liquid.Types.Variance
import Language.Haskell.Liquid.Types.PredType
import Language.Haskell.Liquid.Types.LHSymbol
-- import Language.Fixpoint.Types hiding (panic)
import qualified Language.Fixpoint.Types as F
import qualified Data.HashSet as S 

import BasicTypes
import Name (getName)
-- import DataCon
-- import TyCon
import TysWiredIn

import Language.Haskell.Liquid.GHC.TypeRep
import CoreSyn hiding (mkTyArg)

-- | Horrible hack to support hardwired symbols like
--      `head`, `tail`, `fst`, `snd`
--   and other LH generated symbols that
--   *do not* correspond to GHC Vars and
--   *should not* be resolved to GHC Vars.

isWiredIn :: F.Located LHSymbol -> Bool
isWiredIn x = isWiredInLoc x  || undefined isWiredInName (val x) || isWiredInShape (val x)

isWiredInLoc :: F.Located LHSymbol -> Bool
isWiredInLoc x  = l == l' && l == 0 && c == c' && c' == 0
  where
    (l , c)  = spe (loc x)
    (l', c') = spe (locE x)
    spe l    = (x, y) where (_, x, y) = F.sourcePosElts l

isWiredInName :: F.FixSymbol -> Bool
isWiredInName x = x `S.member` wiredInNames

wiredInNames :: S.HashSet F.FixSymbol
wiredInNames = S.fromList [ "head", "tail", "fst", "snd", "len" ]

isWiredInShape :: LHSymbol -> Bool
isWiredInShape (LHRefSym x) = any (`F.isPrefixOfSym` x) [F.anfPrefix, F.tempPrefix, dcPrefix]
isWiredInShape _ = False
  -- where s        = val x
        -- dcPrefix = "lqdc"

dcPrefix :: F.FixSymbol
dcPrefix = "lqdc"

wiredSortedSyms :: [(F.FixSymbol, F.Sort LHSymbol)]
wiredSortedSyms = [(pappSym n, pappSort n) | n <- [1..pappArity]]

--------------------------------------------------------------------------------
-- | LH Primitive TyCons -------------------------------------------------------
--------------------------------------------------------------------------------

dictionaryVar :: Var
dictionaryVar   = stringVar "tmp_dictionary_var" (ForAllTy (TvBndr dictionaryTyVar Required) $ TyVarTy dictionaryTyVar)

dictionaryTyVar :: TyVar
dictionaryTyVar = stringTyVar "da"

dictionaryBind :: Bind Var
dictionaryBind = Rec [(v, Lam a $ App (Var v) (Type $ TyVarTy a))]
  where
   v = dictionaryVar
   a = dictionaryTyVar

-----------------------------------------------------------------------
-- | LH Primitive TyCons ----------------------------------------------
-----------------------------------------------------------------------


combineProofsName :: String
combineProofsName = "combineProofs"

proofTyConName :: F.Symbol LHSymbol
proofTyConName = "Proof"

--------------------------------------------------------------------------------
-- | Predicate Types for WiredIns ----------------------------------------------
--------------------------------------------------------------------------------

maxArity :: Arity
maxArity = 7

wiredTyCons :: [TyConP]
wiredTyCons  = fst wiredTyDataCons

wiredDataCons :: [Located DataConP]
wiredDataCons = snd wiredTyDataCons

wiredTyDataCons :: ([TyConP] , [Located DataConP])
wiredTyDataCons = (concat tcs, dummyLoc <$> concat dcs)
  where
    (tcs, dcs)  = unzip $ listTyDataCons : map tupleTyDataCons [2..maxArity]

listTyDataCons :: ([TyConP] , [DataConP])
listTyDataCons   = ( [(TyConP l0 c [RTV tyv] [p] [] [Covariant] [Covariant] (Just fsize))]
                   , [(DataConP l0 nilDataCon  [RTV tyv] [p] [] [] []    lt False (F.AS . LHRefSym $ wiredInName) l0)
                     ,(DataConP l0 consDataCon [RTV tyv] [p] [] [] cargs lt False (F.AS . LHRefSym $ wiredInName) l0)])
    where
      l0         = F.dummyPos "LH.Bare.listTyDataCons"
      c          = listTyCon
      [tyv]      = tyConTyVarsDef c
      t          = rVar tyv :: RSort
      fld        = "fldList"
      xHead      = "head"
      xTail      = "tail"
      p          = PV "p" (PVProp t) (F.vv Nothing) [(t, fld, F.EVar . F.AS . LHRefSym $ fld)]
      px         = pdVarReft $ PV "p" (PVProp t) (F.vv Nothing) [(t, fld, F.EVar xHead)]
      lt         = rApp c [xt] [rPropP [] $ pdVarReft p] mempty
      xt         = rVar tyv
      xst        = rApp c [RVar (RTV tyv) px] [rPropP [] $ pdVarReft p] mempty
      cargs      = [(xTail, xst), (xHead, xt)]
      fsize      = SymSizeFun (dummyLoc "len")

wiredInName :: F.FixSymbol
wiredInName = "WiredIn"

tupleTyDataCons :: Int -> ([TyConP] , [DataConP])
tupleTyDataCons n = ( [(TyConP   l0 c  (RTV <$> tyvs) ps [] tyvarinfo pdvarinfo Nothing)]
                    , [(DataConP l0 dc (RTV <$> tyvs) ps [] []  cargs  lt False
                        (F.AS . LHRefSym $ wiredInName) l0)])
  where
    tyvarinfo     = replicate n     Covariant
    pdvarinfo     = replicate (n-1) Covariant
    l0            = F.dummyPos "LH.Bare.tupleTyDataCons"
    c             = tupleTyCon   Boxed n
    dc            = tupleDataCon Boxed n
    tyvs@(tv:tvs) = tyConTyVarsDef c
    (ta:ts)       = (rVar <$> tyvs) :: [RSort]
    flds          = mks "fld_Tuple"
    fld           = "fld_Tuple"
    x1:xs         = mks ("x_Tuple" ++ show n)
    fldSym        = F.AS . LHRefSym $ fld
    fldsSym       = F.AS . LHRefSym <$> flds
    x1Sym:xsSym   = F.AS . LHRefSym <$> x1:xs
    ps            = mkps pnames (ta:ts) ((fld, F.EVar fldSym) : zip flds (F.EVar <$> fldsSym))
    ups           = uPVar <$> ps
    pxs           = mkps pnames (ta:ts) ((fld, F.EVar x1Sym) : zip flds (F.EVar <$> xsSym))
    lt            = rApp c (rVar <$> tyvs) (rPropP [] . pdVarReft <$> ups) mempty
    xts           = zipWith (\v p -> RVar (RTV v) (pdVarReft p)) tvs pxs
    cargs         = reverse $ (x1Sym, rVar tv) : zip xsSym xts
    pnames        = mks_ "p"
    mks  x        = (\i -> F.symbol (x++ show i)) <$> [1..n]
    mks_ x        = (\i -> F.symbol (x++ show i)) <$> [2..n]


mkps :: [F.FixSymbol]
     -> [t] -> [(F.FixSymbol, F.Expr LHSymbol)] -> [PVar t]
mkps ns (t:ts) ((f,x):fxs) = reverse $ mkps_ ns ts fxs [(t, f, x)] []
mkps _  _      _           = panic Nothing "Bare : mkps"

mkps_ :: [F.FixSymbol]
      -> [t]
      -> [(F.FixSymbol, F.Expr LHSymbol)]
      -> [(t, F.FixSymbol, F.Expr LHSymbol)]
      -> [PVar t]
      -> [PVar t]
mkps_ []     _       _          _    ps = ps
mkps_ (n:ns) (t:ts) ((f, x):xs) args ps = mkps_ ns ts xs (a:args) (p:ps)
  where
    p                                   = PV n (PVProp t)
                                          (F.vv Nothing)
                                          args
    a                                   = (t, f, x)
mkps_ _     _       _          _    _ = panic Nothing "Bare : mkps_"


--------------------------------------------------------------------------------
isDerivedInstance :: Ghc.ClsInst -> Bool 
--------------------------------------------------------------------------------
isDerivedInstance i = F.notracepp ("IS-DERIVED: " ++ F.showpp classSym) 
                    $ S.member classSym derivingClasses 
  where 
    classSym        = F.symbol . getName . Ghc.is_cls $ i
  
derivingClasses :: S.HashSet F.FixSymbol 
derivingClasses = S.fromList 
  [ "GHC.Classes.Eq"
  , "GHC.Classes.Ord"
  , "GHC.Enum.Enum"
  , "GHC.Show.Show"
  , "GHC.Read.Read"
  , "GHC.Base.Monad"
  , "GHC.Base.Applicative"
  , "GHC.Base.Functor"
  , "Data.Foldable.Foldable"
  , "Data.Traversable.Traversable"
  -- , "GHC.Enum.Bounded"
  -- , "GHC.Base.Monoid"
  ]
