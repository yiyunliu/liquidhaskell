{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE OverloadedStrings         #-}

module Language.Haskell.Liquid.GHC.Interface (

  -- * extract all information needed for verification
    getGhcInfo,
    runLiquidGhc,

  -- * printer
    pprintCBs
  ) where

import Prelude hiding (error)

import qualified Outputable as O
import GHC hiding (Target, desugarModule, Located)
import qualified GHC
import GHC.Paths (libdir)

import Bag
import Class
import CoreMonad
import CoreSyn
import DataCon
import DriverPhases
import DriverPipeline
import DynFlags
import ErrUtils
import HscTypes hiding (Target)
import IdInfo
import InstEnv
import Var

import Control.Exception
import Control.Monad

import Data.List hiding (intersperse)
import Data.Maybe
-- import Data.Function (on)

import qualified Data.HashSet        as S
import qualified Data.HashMap.Strict as M
import qualified Data.IntSet         as I

import System.Console.CmdArgs.Verbosity hiding (Loud)
import System.Directory
import System.FilePath
import System.IO.Temp

import Text.PrettyPrint.HughesPJ

import Language.Fixpoint.Types hiding (Error, Result, Expr)
import Language.Fixpoint.Misc

import Language.Haskell.Liquid.Bare
import Language.Haskell.Liquid.GHC.Misc
import qualified Language.Haskell.Liquid.Measure as Ms
import Language.Haskell.Liquid.Misc
import Language.Haskell.Liquid.Parse
import Language.Haskell.Liquid.Transforms.ANF
import Language.Haskell.Liquid.Types
import Language.Haskell.Liquid.Types.PrettyPrint
import Language.Haskell.Liquid.Types.Visitors
import Language.Haskell.Liquid.UX.CmdLine
import Language.Haskell.Liquid.UX.Tidy
import Language.Fixpoint.Utils.Files

--------------------------------------------------------------------------------
-- GHC Interface Pipeline ------------------------------------------------------
--------------------------------------------------------------------------------

getGhcInfo :: Maybe HscEnv -> Config -> FilePath -> IO (GhcInfo, HscEnv)
getGhcInfo hscEnv cfg0 target = do
  tryIgnore "create temp directory" $
    createDirectoryIfMissing False $ tempDirectory target
  (cfg, name, tgtSpec) <- parseRootTarget cfg0 target
  runLiquidGhc hscEnv cfg $ getGhcInfo' cfg target name tgtSpec

getGhcInfo' :: Config -> FilePath -> ModName -> Ms.BareSpec -> Ghc (GhcInfo, HscEnv)
getGhcInfo' cfg target name tgtSpec = do
  paths     <- importPaths <$> getSessionDynFlags
  liftIO     $ whenLoud $ putStrLn $ "paths = " ++ show paths
  impSpecs  <- findAndLoadTargets cfg paths name target
  modGuts   <- makeMGIModGuts target
  hscEnv    <- getSession
  coreBinds <- liftIO $ anormalize (not $ nocaseexpand cfg) hscEnv modGuts
  logicMap  <- liftIO makeLogicMap
  let dataCons = concatMap (map dataConWorkId . tyConDataCons) (mgi_tcs modGuts)
  let impVs = importVars coreBinds ++ classCons (mgi_cls_inst modGuts)
  let defVs = definedVars coreBinds
  let useVs = readVars coreBinds
  let letVs = letVars coreBinds
  let derVs = derivedVars coreBinds $ ((is_dfun <$>) <$>) $ mgi_cls_inst modGuts
  (spc, imps, incs) <- moduleSpec cfg coreBinds (impVs ++ defVs) letVs name modGuts tgtSpec logicMap impSpecs
  liftIO $ whenLoud $ putStrLn $ "Module Imports: " ++ show imps
  hqualFiles <- moduleHquals modGuts paths target imps incs
  let info    = GI target (getModName name) hscEnv coreBinds derVs impVs (letVs ++ dataCons) useVs hqualFiles imps incs spc
  hscEnv'    <- getSession
  return (info, hscEnv')

--------------------------------------------------------------------------------
-- Configure GHC for Liquid Haskell --------------------------------------------
--------------------------------------------------------------------------------

runLiquidGhc :: Maybe HscEnv -> Config -> Ghc a -> IO a
runLiquidGhc hscEnv cfg act =
  withSystemTempDirectory "liquid" $ \tmp ->
    runGhc (Just libdir) $ do
      maybe (return ()) setSession hscEnv
      df <- getSessionDynFlags
      df' <- parseUserDynFlags df $ ghcOptions cfg
      df'' <- liftIO $ configureDynFlags cfg tmp df'
      _ <- setSessionDynFlags df''
      defaultCleanupHandler df'' act

-- | Ignore warnings generated when applying user DynFlags (like the -O and
-- interactive-mode conflict). Jump ahead and disable -Werr here, if set, to
-- ensure DynFlag merge warnings don't kill us.
parseUserDynFlags :: DynFlags -> [String] -> Ghc DynFlags
parseUserDynFlags df opts = do
  setSessionDynFlags dfNoWerr
  (df', _, _) <- parseDynamicFlags dfNoWerr $ cmdLineLoc <$> opts
  setSessionDynFlags df
  return df'
  where
    dfNoWerr   = df `gopt_unset` Opt_WarnIsError
    cmdLineLoc = mkGeneralLocated "<command line>"

-- | Configure LiquidHaskell-specific DynFlags. Take care to set *safe*
-- values for everything we should, so we aren't relying on any defaults
-- that might get overwritten by the user's GHC options (which may be
-- generated via Cabal and not intended for us).
configureDynFlags :: Config -> FilePath -> DynFlags -> IO DynFlags
configureDynFlags cfg tmp df = do
  loud <- isLoud
  let defaults = defaultDynFlags $ settings df
  let dfWays = nub $ ways defaults ++
                 if WayDyn `elem` ways df
                   then [WayDyn]
                   else []
  -- MAINTENANCE NOTE: Please go through these again for each new GHC version!
  return $ df
    { ghcMode                  = CompManager
    , ghcLink                  = LinkInMemory
    -- FIXME: this *should* be HscNothing, but that prevents us from
    -- looking up *unexported* names in another source module..
    -- prevent GHC from printing anything, unless in Loud mode
    , hscTarget                = HscInterpreted -- HscNothing
    -- , settings              = <user>
    -- , sigOf                 = <user>
    , verbosity                = if loud
                                   then max 3 $ verbosity df
                                   else 0
    -- TODO: Perhaps turning down simplifier phases could help
    -- our analysis of the core?
    , optLevel                 = optLevel defaults
    , simplPhases              = simplPhases defaults
    , maxSimplIterations       = maxSimplIterations defaults
    , ruleCheck                = ruleCheck defaults
    , strictnessBefore         = strictnessBefore defaults
    -- TODO: Should we take advantage of parallelism here?
    , parMakeCount             = parMakeCount defaults
    , enableTimeStats          = enableTimeStats defaults
    -- , ghcHeapSize           = <user>
    -- , maxRelevantBinds      = <user>
    , simplTickFactor          = simplTickFactor defaults
    , specConstrThreshold      = specConstrThreshold defaults
    , specConstrCount          = specConstrCount defaults
    , specConstrRecursive      = specConstrRecursive defaults
    , liberateCaseThreshold    = liberateCaseThreshold defaults
    , floatLamArgs             = floatLamArgs defaults
    , historySize              = historySize defaults
    -- , cmdlineHcIncludes     = <user>
    , importPaths              = nub $ idirs cfg ++ importPaths df
    -- , mainModIs             = <user>
    -- , mainFunIs             = <user>
    -- , ctxtStkDepth          = <user>
    -- , tyFunStkDepth         = <user>
    -- , thisPackage           = <user>
    , ways                     = dfWays
    , buildTag                 = mkBuildTag dfWays
    , rtsBuildTag              = mkBuildTag dfWays
    , splitInfo                = splitInfo defaults
    , objectDir                = Just tmp
    -- , dylibInstallName      = <user>
    , hiDir                    = Just tmp
    , stubDir                  = Just tmp
    , dumpDir                  = Just tmp
    -- , objectSuf             = <user>
    -- , hcSuf                 = <user>
    -- , hiSuf                 = <user>
    -- , canGenerateDynamicToo = <user>
    -- , dynObjectSuf          = <user>
    -- , dynHiSuf              = <user>
    , dllSplitFile             = dllSplitFile defaults
    , dllSplit                 = dllSplit defaults
    , outputFile               = outputFile defaults
    , dynOutputFile            = dynOutputFile defaults
    , outputHi                 = outputHi defaults
    -- , dynLibLoader          = <user>
    , dumpPrefix               = dumpPrefix defaults
    , dumpPrefixForce          = dumpPrefixForce defaults
    -- , ldInputs              = <user>
    , includePaths             = nub $ idirs cfg ++ includePaths df
    , libraryPaths             = nub $ idirs cfg ++ libraryPaths df
    , frameworkPaths           = nub $ idirs cfg ++ frameworkPaths df
    -- , cmdlineFrameworks     = <user>
    , rtsOpts                  = rtsOpts defaults
    , rtsOptsEnabled           = rtsOptsEnabled defaults
    , hpcDir                   = tmp
    -- , pluginModNames        = <user>
    -- , pluginModNameOpts     = <user>
    -- , hooks                 = <user>
    , depMakefile              = depMakefile defaults
    , depIncludePkgDeps        = depIncludePkgDeps defaults
    , depExcludeMods           = depExcludeMods defaults
    , depSuffixes              = depSuffixes defaults
    -- , extraPkgConfs         = <user>
    , packageFlags             = ExposePackage (PackageArg "ghc-prim")
                                               (ModRenaming True [])
                                 : packageFlags df
    -- , packageEnv            = <user>
    -- , pkgDatabase           = <user>
    -- , pkgState              = <user>
    -- , filesToClean          = <user>
    -- , dirsToClean           = <user>
    -- , filesToNotIntermediateClean = <user>
    -- , nextTempSuffix        = <user>
    -- , generatedDumps        = <user>
    , dumpFlags                = dumpFlags defaults
    -- (generalFlags are handled below)
    , warningFlags             = if loud
                                   then warningFlags df
                                   else I.empty
    -- , language              = <user>
    -- , safeHaskell           = <user>
    -- , safeInfer             = <user>
    -- , safeInferred          = <user>
    -- , thOnLoc               = <user>
    -- , newDerivOnLoc         = <user>
    -- , overlapInstLoc        = <user>
    -- , incoherentOnLoc       = <user>
    -- , pkgTrustOnLoc         = <user>
    -- , warnSafeOnLoc         = <user>
    -- , warnUnsafeOnLoc       = <user>
    -- , trustworthyOnLoc      = <user>
    -- , extensionFlags        = <user> (also see below)
    , ufCreationThreshold      = ufCreationThreshold defaults
    , ufUseThreshold           = ufUseThreshold defaults
    , ufFunAppDiscount         = ufFunAppDiscount defaults
    , ufDictDiscount           = ufDictDiscount defaults
    , ufKeenessFactor          = ufKeenessFactor defaults
    , ufDearOp                 = ufDearOp defaults
    , maxWorkerArgs            = maxWorkerArgs defaults
    , ghciHistSize             = ghciHistSize defaults
    , log_action               = if loud
                                   then defaultLogAction
                                   else \_ _ _ _ _ -> return ()
    , flushOut                 = flushOut defaults
    , flushErr                 = flushErr defaults
    , haddockOptions           = haddockOptions defaults
    , ghciScripts              = ghciScripts defaults
    -- , pprUserLength         = <user>
    -- , pprCols               = <user>
    , traceLevel               = if loud
                                   then max 1 $ traceLevel df
                                   else 0
    -- , useUnicode            = <user>
    -- TODO: This used to be ProfAutoCalls but was commented out;
    -- why?
    , profAuto                 = profAuto defaults
    , interactivePrint         = interactivePrint defaults
    -- , llvmVersion           = <user>
    -- , nextWrapperNum        = <user>
    -- , sseVersion            = <user>
    -- , avx                   = <user>
    -- , avx2                  = <user>
    -- , avx512cd              = <user>
    -- , avx512er              = <user>
    -- , avx512f               = <user>
    -- , avx512pf              = <user>
    -- , rtldInfo              = <user>
    -- , rtccInfo              = <user>
    -- , maxInlineAllocSize    = <user>
    -- , maxInlineMemcpyInsns  = <user>
    -- , maxInlineMemsetInsns  = <user>
    } `xopt_set` Opt_MagicHash
      `xopt_set` Opt_DeriveGeneric
      `xopt_set` Opt_StandaloneDeriving
      `gopt_set` Opt_ImplicitImportQualified
      `gopt_set` Opt_PIC
      `gopt_unset` Opt_DumpToFile
      `gopt_unset` Opt_DoCoreLinting
      `gopt_unset` Opt_DoStgLinting
      `gopt_unset` Opt_DoCmmLinting
      `gopt_unset` Opt_DoAsmLinting
      `gopt_unset` Opt_DoAnnotationLinting
      `gopt_unset` Opt_WarnIsError
      `gopt_unset` Opt_WriteInterface
      `gopt_unset` Opt_SplitObjs
      `gopt_unset` Opt_GhciSandbox
      `gopt_unset` Opt_GhciHistory
      `gopt_unset` Opt_Hpc

--------------------------------------------------------------------------------
-- Parse, Find, & Load Targets -------------------------------------------------
--------------------------------------------------------------------------------

parseRootTarget :: Config -> FilePath -> IO (Config, ModName, Ms.BareSpec)
parseRootTarget cfg0 target = do
  (name, tgtSpec) <- parseSpec target
  cfg <- withPragmas cfg0 target $ Ms.pragmas tgtSpec
  return (cfg, ModName Target $ getModName name, tgtSpec)

findAndLoadTargets :: Config -> [FilePath] -> ModName -> FilePath -> Ghc [(ModName, Ms.BareSpec)]
findAndLoadTargets cfg paths name target = do
  setTargets . return =<< guessTarget target Nothing
  impNames <- allDepNames <$> depanal [] False
  impSpecs <- getSpecs cfg paths name target impNames [Spec, Hs, LHs]
  liftIO $ whenNormal $ donePhase Loud "Parsed All Specifications"
  compileCFiles =<< liftIO (foldM (\c (f,_,s) -> withPragmas c f (Ms.pragmas s)) cfg impSpecs)
  impSpecs' <- forM impSpecs $ \(f, n, s) -> do
                 unless (isSpecImport n) $
                   addTarget =<< guessTarget f Nothing
                 return (n, s)
  load LoadAllTargets
  liftIO $ whenNormal $ donePhase Loud "Loaded Targets"
  return impSpecs'

allDepNames :: [ModSummary] -> [String]
allDepNames = concatMap (map declNameString . ms_textual_imps)

declNameString :: GHC.Located (ImportDecl RdrName) -> String
declNameString = moduleNameString . unLoc . ideclName . unLoc

compileCFiles :: Config -> Ghc ()
compileCFiles cfg = do
  df  <- getSessionDynFlags
  setSessionDynFlags $ df { includePaths = nub $ idirs cfg ++ includePaths df
                          , importPaths  = nub $ idirs cfg ++ importPaths df
                          , libraryPaths = nub $ idirs cfg ++ libraryPaths df }
  hsc <- getSession
  os  <- mapM (\x -> liftIO $ compileFile hsc StopLn (x,Nothing)) (nub $ cFiles cfg)
  df  <- getSessionDynFlags
  void $ setSessionDynFlags $ df { ldInputs = map (FileOption "") os ++ ldInputs df }

--------------------------------------------------------------------------------
-- Assemble Information for Spec Extraction ------------------------------------
--------------------------------------------------------------------------------

makeMGIModGuts :: FilePath -> Ghc MGIModGuts
makeMGIModGuts f = do
  modGraph <- getModuleGraph
  case find (\m -> not (isBootSummary m) && f == msHsFilePath m) modGraph of
    Just modSummary -> do
      parsed   <- parseModule modSummary
      modGuts  <- coreModule <$> (desugarModule =<< typecheckModule (ignoreInline parsed))
      let deriv = Just $ instEnvElts $ mg_inst_env modGuts
      return $! miModGuts deriv modGuts
    Nothing ->
      panic Nothing "Ghc Interface: Unable to get GhcModGuts"

makeLogicMap :: IO (Either Error LogicMap)
makeLogicMap = do
  lg    <- getCoreToLogicPath
  lspec <- readFile lg
  return $ parseSymbolToLogic lg lspec

--------------------------------------------------------------------------------
-- Extract Ids -----------------------------------------------------------------
--------------------------------------------------------------------------------

classCons :: Maybe [ClsInst] -> [Id]
classCons Nothing   = []
classCons (Just cs) = concatMap (dataConImplicitIds . head . tyConDataCons . classTyCon . is_cls) cs

derivedVars :: CoreProgram -> Maybe [DFunId] -> [Id]
derivedVars cbs (Just fds) = concatMap (derivedVs cbs) fds
derivedVars _   Nothing    = []

derivedVs :: CoreProgram -> DFunId -> [Id]
derivedVs cbs fd = concatMap bindersOf cbs' ++ deps
  where
    cbs'           = filter f cbs
    f (NonRec x _) = eqFd x
    f (Rec xes)    = any eqFd (fst <$> xes)
    eqFd x         = varName x == varName fd
    deps           = concatMap unfoldDep unfolds
    unfolds        = unfoldingInfo . idInfo <$> concatMap bindersOf cbs'

unfoldDep :: Unfolding -> [Id]
unfoldDep (DFunUnfolding _ _ e)         = concatMap exprDep e
unfoldDep (CoreUnfolding {uf_tmpl = e}) = exprDep e
unfoldDep _                             = []

exprDep :: CoreExpr -> [Id]
exprDep = freeVars S.empty

importVars :: CoreProgram -> [Id]
importVars = freeVars S.empty

definedVars :: CoreProgram -> [Id]
definedVars = concatMap defs
  where
    defs (NonRec x _) = [x]
    defs (Rec xes)    = map fst xes

--------------------------------------------------------------------------------
-- Find & Parse Specs ----------------------------------------------------------
--------------------------------------------------------------------------------
type FileSpec = (FilePath, ModName, Ms.BareSpec)

getSpecs :: Config -> [FilePath] -> ModName -> FilePath -> [String] -> [Ext] -> Ghc [FileSpec]
getSpecs cfg paths name target names exts = do
  fSpecs <- getSpecs' cfg paths target names exts
  -- liftIO $ putStrLn $ "getSpecs    [RAW]: " ++ show [(f, n) | (f, n, _) <- fSpecs]
  let fSpecs'  = normalizeFileSpec fSpecs
  let fSpecs'' = filter ((/= getModString name) . getModString . snd3) fSpecs'
  -- liftIO $ putStrLn $ "getSpecs [NORMAL]: " ++ showTable [(n, text f) | (f, n, _) <- fSpecs']
  return fSpecs''

-- showTable = render . pprintKVs Full . sortBy (compare `on` fst)

getSpecs' :: Config -> [FilePath] -> FilePath -> [String] -> [Ext] -> Ghc [FileSpec]
getSpecs' cfg paths target names exts = do
  fs'     <- sortNub <$> moduleImports exts paths names
  patSpec <- getPatSpec paths $ totality cfg
  rlSpec  <- getRealSpec paths $ not $ linear cfg
  let fs   = patSpec ++ rlSpec ++ fs'
  transParseSpecs exts paths (S.singleton target) mempty (map snd fs \\ [target])
  -- liftIO $ putStrLn $ "getSpecs [NORMAL]: " ++ showTable [(n, text f) | (f, n, _) <- fSpecs]
  -- return fSpecs
  -- where
  --   showTable = render . pprintKVs Full . sortBy (compare `on` fst)

normalizeFileSpec :: [FileSpec] -> [FileSpec]
normalizeFileSpec = concat
                  . M.elems
                  . fmap partSpecs
                  . groupMap (show . snd3)

partSpecs :: [FileSpec] -> [FileSpec]
partSpecs fs = case partition isSpecFile fs of
                 (sFs, [] ) -> sFs
                 (_  , cFs) -> cFs

isSpecFile :: FileSpec -> Bool
isSpecFile (f, _, _)
  | isExtFile Spec f = True
  | otherwise        = False

getPatSpec :: [FilePath] -> Bool -> Ghc [(String, FilePath)]
getPatSpec paths totalitycheck
 | totalitycheck = map (patErrorName,) . maybeToList <$> moduleFile paths patErrorName Spec
 | otherwise     = return []
 where
  patErrorName = "PatErr"

getRealSpec :: [FilePath] -> Bool -> Ghc [(String, FilePath)]
getRealSpec paths freal
  | freal     = map (realSpecName,)    . maybeToList <$> moduleFile paths realSpecName    Spec
  | otherwise = map (notRealSpecName,) . maybeToList <$> moduleFile paths notRealSpecName Spec
  where
    realSpecName    = "Real"
    notRealSpecName = "NotReal"

transParseSpecs :: [Ext] -> [FilePath] -> S.HashSet FilePath -> [FileSpec] -> [FilePath]
                -> Ghc [FileSpec]
transParseSpecs _ _ _ specs [] = return specs
transParseSpecs exts paths seenFiles specs newFiles = do
  newSpecs      <- liftIO $ mapM (\f -> addFst3 f <$> parseSpec f) newFiles
  impFiles      <- moduleImports exts paths $ specsImports newSpecs
  let seenFiles' = seenFiles `S.union` S.fromList newFiles
  let specs'     = specs ++ map (third3 noTerm) newSpecs
  let newFiles'  = [f | (_, f) <- impFiles, not (f `S.member` seenFiles')]
  transParseSpecs exts paths seenFiles' specs' newFiles'
  where
    specsImports ss = nub $ concatMap (map symbolString . Ms.imports . thd3) ss
    noTerm spec = spec { Ms.decr = mempty, Ms.lazy = mempty, Ms.termexprs = mempty }

parseSpec :: FilePath -> IO (ModName, Ms.BareSpec)
parseSpec file = either throw return . specParser file =<< readFile file

specParser :: FilePath -> String -> Either Error (ModName, Ms.BareSpec)
specParser f str
  | isExtFile Spec   f = specSpecificationP f str
  | isExtFile Hs     f = hsSpecificationP   f str
  | isExtFile HsBoot f = hsSpecificationP   f str
  | isExtFile LHs    f = lhsSpecificationP  f str
  | otherwise          = panic Nothing $ "SpecParser: Cannot Parse File " ++ f


moduleSpec :: GhcMonad m
           => Config
           -> [CoreBind]
           -> [Var]
           -> [Var]
           -> ModName
           -> MGIModGuts
           -> Ms.Spec (Located BareType) LocSymbol
           -> Either Error LogicMap
           -> [(ModName, Ms.BareSpec)]
           -> m (GhcSpec, [String], [FilePath])
moduleSpec cfg cbs vars letVs tgtMod mgi tgtSpec lm impSpecs = do
  let tgtCxt = IIModule $ getModName tgtMod
  let impCxt = map (IIDecl . qualImportDecl . getModName . fst) impSpecs
  setContext (tgtCxt : impCxt)
  hsc <- getSession
  let impNames = map (getModString . fst) impSpecs
  let exports  = mgi_exports mgi
  let specs = (tgtMod, tgtSpec) : impSpecs
  let imps  = sortNub $ impNames ++ [ symbolString x | (_, sp) <- specs, x <- Ms.imports sp ]
  ghcSpec <- liftIO $ makeGhcSpec cfg tgtMod cbs vars letVs exports hsc lm specs
  return (ghcSpec, imps, Ms.includes tgtSpec)

moduleHquals :: MGIModGuts
             -> [FilePath]
             -> FilePath
             -> [String]
             -> [FilePath]
             -> Ghc [FilePath]
moduleHquals mgi paths target imps incs = do
  hqs   <- specIncludes Hquals paths incs
  hqs'  <- moduleImports [Hquals] paths (mgi_namestring mgi : imps)
  hqs'' <- liftIO $ filterM doesFileExist [extFileName Hquals target]
  return $ sortNub $ hqs'' ++ hqs ++ (snd <$> hqs')


moduleImports :: [Ext] -> [FilePath] -> [String] -> Ghc [(String, FilePath)]
moduleImports exts paths names = liftM concat $ forM names $ \name ->
  map (name,) . catMaybes <$> mapM (moduleFile paths name) exts

moduleFile :: [FilePath] -> String -> Ext -> Ghc (Maybe FilePath)
moduleFile paths name ext
  | ext `elem` [Hs, LHs] = do
    graph <- getModuleGraph
    case find (\m -> not (isBootSummary m) &&
                     name == moduleNameString (ms_mod_name m)) graph of
      Nothing -> liftIO $ getFileInDirs (extModuleName name ext) paths
      Just ms -> return $ normalise <$> ml_hs_file (ms_location ms)
  | otherwise = liftIO $ getFileInDirs (extModuleName name ext) paths

specIncludes :: Ext -> [FilePath] -> [FilePath] -> Ghc [FilePath]
specIncludes ext paths reqs = do
  let libFile = extFileNameR ext $ symbolString preludeName
  let incFiles = catMaybes $ reqFile ext <$> reqs
  liftIO $ forM (libFile : incFiles) $ \f -> do
    mfile <- getFileInDirs f paths
    case mfile of
      Just file -> return file
      Nothing -> panic Nothing $ "cannot find " ++ f ++ " in " ++ show paths

reqFile :: Ext -> FilePath -> Maybe FilePath
reqFile ext s
  | isExtFile ext s = Just s
  | otherwise = Nothing

--------------------------------------------------------------------------------
-- Pretty Printing -------------------------------------------------------------
--------------------------------------------------------------------------------

instance PPrint GhcSpec where
  pprintTidy k spec = vcat
    [ "******* Target Variables ********************"
    , pprintTidy k $ tgtVars spec
    , "******* Type Signatures *********************"
    , pprintLongList k (tySigs spec)
    , "******* Assumed Type Signatures *************"
    , pprintLongList k (asmSigs spec)
    , "******* DataCon Specifications (Measure) ****"
    , pprintLongList k (ctors spec)
    , "******* Measure Specifications **************"
    , pprintLongList k (meas spec)                   ]

instance PPrint GhcInfo where
  pprintTidy k info = vcat
    [ "*************** Imports *********************"
    , intersperse comma $ text <$> imports info
    , "*************** Includes ********************"
    , intersperse comma $ text <$> includes info
    , "*************** Imported Variables **********"
    , pprDoc $ impVars info
    , "*************** Defined Variables ***********"
    , pprDoc $ defVars info
    , "*************** Specification ***************"
    , pprintTidy k $ spec info
    , "*************** Core Bindings ***************"
    , pprintCBs $ cbs info                          ]


-- RJ: the silly guards below are to silence the unused-var checker

pprintCBs :: [CoreBind] -> Doc
pprintCBs
  | True      = pprintCBsTidy
  | otherwise = pprintCBsVerbose
  where
    pprintCBsTidy    = pprDoc . tidyCBs
    pprintCBsVerbose = text . O.showSDocDebug unsafeGlobalDynFlags . O.ppr . tidyCBs

instance Show GhcInfo where
  show = showpp

instance PPrint TargetVars where
  pprintTidy _ AllVars   = text "All Variables"
  pprintTidy k (Only vs) = text "Only Variables: " <+> pprintTidy k vs

------------------------------------------------------------------------
-- Dealing with Errors ---------------------------------------------------
------------------------------------------------------------------------

instance Result SourceError where
  result = (`Crash` "Invalid Source")
         . concatMap errMsgErrors
         . bagToList
         . srcErrorMessages

errMsgErrors :: ErrMsg -> [TError t]
errMsgErrors e = [ ErrGhc (errMsgSpan e) (pprint e)]
