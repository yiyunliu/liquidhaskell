{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Haskell.Liquid.Spec.Env (
    -- * Specificaton Building Monad
    SpecM
  , runSpecM
    -- * Error Handling
  , MonadError(..)
  , MonadErrors(..)
    -- * Operations in SpecM
  , MonadSpec(..)
  ) where

import GHC

import DynFlags
import Exception

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Reader (liftCatch)

import Data.List

import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S

import Language.Fixpoint.Types hiding (Error)

import Language.Haskell.Liquid.Types
import Language.Haskell.Liquid.Types.Bounds
import Language.Haskell.Liquid.UX.Tidy ()

import qualified Language.Haskell.Liquid.Measure as Ms

--------------------------------------------------------------------------------
-- | Specification Building Monad ----------------------------------------------
--------------------------------------------------------------------------------

newtype SpecM a
  = SpecM
    { unSpecM :: ReaderT SpecEnv (ExceptT [Error] Ghc) a
    }
  deriving (Functor, Applicative, Monad, MonadIO)

data SpecEnv
  = SpecEnv
    { specEnvCurrentModule :: !(Maybe Module)
    , specEnvExternBounds  :: !RBEnv
    , specEnvLocalBounds   :: !RBEnv
    , specEnvExternAliases :: !RTEnv
    , specEnvLocalAliases  :: !RTEnv
    , specEnvFixScope      :: !(S.HashSet Symbol)
    }


instance ExceptionMonad SpecM where
  gcatch act handle = SpecM $ do
    env <- ask
    lift $ (either throwError return =<<) $ lift $
      runSpecM' act env `gcatch` \e -> runSpecM' (handle e) env
  gmask f = SpecM $ do
    env <- ask
    lift $ (either throwError return =<<) $ lift $ gmask $ \restore ->
      let restore' = liftGhc . restore . (either (throw . WrapEx) return =<<)
                             . (`runSpecM'` env)
      in  runSpecM' (f restore' `gcatch` (throwErrors . unWrapEx)) env

newtype WrapEx = WrapEx { unWrapEx :: [Error] } deriving (Show)

instance Exception WrapEx


instance HasDynFlags SpecM where
  getDynFlags = liftGhc getDynFlags

instance GhcMonad SpecM where
  getSession = liftGhc getSession
  setSession = liftGhc . setSession

runSpecM :: SpecM a
         -> GlobalSpec
         -> Maybe Module
         -> Ms.BareSpec
         -> Ghc (Either [Error] a)
runSpecM act extern mod bspec = runSpecM' act $ SpecEnv
  { specEnvCurrentModule = mod
  , specEnvExternBounds  = bounds extern
  , specEnvLocalBounds   = mempty
  , specEnvExternAliases = aliases extern
  , specEnvLocalAliases  = mempty
  , specEnvFixScope      = S.fromList $
                             bareMeasureNames bspec ++ M.keys (meas extern)
  }

bareMeasureNames :: Ms.BareSpec -> [Symbol]
bareMeasureNames bspec = concat
  [ val . name <$> Ms.measures bspec
  , val . name <$> Ms.cmeasures bspec
  , val . name <$> Ms.imeasures bspec
  , val <$> S.toList (Ms.hmeas bspec)
  , concatMap (concatMap ((val . fst <$>) . snd) . tycDCons . val) $
      Ms.dataDecls bspec
  ]

runSpecM' :: SpecM a -> SpecEnv -> Ghc (Either [Error] a)
runSpecM' act env = runExceptT (runReaderT (unSpecM act) env)

--------------------------------------------------------------------------------
-- | Error Handling ------------------------------------------------------------
--------------------------------------------------------------------------------

instance MonadError Error SpecM where
  throwError = throwErrors . return
  catchError act handle = catchErrors act (handle . head)

class Monad m => MonadErrors e m | m -> e where
  throwErrors :: [e] -> m a
  catchErrors :: m a -> ([e] -> m a) -> m a

instance MonadErrors Error SpecM where
  throwErrors = SpecM . throwError
  catchErrors act handle = SpecM $ catchError (unSpecM act) $ unSpecM . handle

instance MonadErrors e m => MonadErrors e (ReaderT r m) where
  throwErrors = lift . throwErrors
  catchErrors = liftCatch catchErrors

--------------------------------------------------------------------------------
-- | Operations in SpecM -------------------------------------------------------
--------------------------------------------------------------------------------

class Monad m => MonadSpec m where
  liftGhc :: Ghc a -> m a

  getCurrentModule :: m (Maybe Module)

  withLocalBounds :: RBEnv -> m a -> m a
  lookupBound :: Symbol -> m (Maybe RBound)

  withLocalAliases :: RTEnv -> m a -> m a
  lookupExprAlias :: Symbol -> m (Maybe (RTAlias Symbol Expr))
  lookupTypeAlias :: Symbol -> m (Maybe (RTAlias RTyVar SpecType))

  withFixScope :: [Symbol] -> m a -> m a
  isInFixScope :: Symbol -> m Bool

instance MonadSpec SpecM where
  liftGhc = SpecM . lift . lift

  getCurrentModule = SpecM $ asks specEnvCurrentModule

  withLocalBounds bounds = SpecM . withReaderT adj . unSpecM
    where
      adj env = env { specEnvLocalBounds = bounds }
  lookupBound s = SpecM $ asks $ \env ->
    M.lookup (dummyLoc s) (specEnvLocalBounds env) <|>
    M.lookup (dummyLoc s) (specEnvExternBounds env)

  withLocalAliases aliases = SpecM . withReaderT adj . unSpecM
    where
      adj env = env { specEnvLocalAliases = aliases }
  lookupExprAlias s = SpecM $ asks $ \env ->
    M.lookup s (exprAliases $ specEnvLocalAliases env) <|>
    M.lookup s (exprAliases $ specEnvExternAliases env)
  lookupTypeAlias s = SpecM $ asks $ \env ->
    M.lookup s (typeAliases $ specEnvLocalAliases env) <|>
    M.lookup s (typeAliases $ specEnvExternAliases env)

  withFixScope fs = SpecM . withReaderT adj . unSpecM
    where
      adj env = env
        { specEnvFixScope = foldl' (flip S.insert) (specEnvFixScope env) fs
        }
  isInFixScope s = SpecM $ asks $ (s `S.member`) . specEnvFixScope

instance MonadSpec m => MonadSpec (ReaderT r m) where
  liftGhc = lift . liftGhc

  getCurrentModule = lift getCurrentModule

  withLocalBounds = liftWithReaderT . withLocalBounds
  lookupBound = lift . lookupBound

  withLocalAliases = liftWithReaderT . withLocalAliases
  lookupExprAlias = lift . lookupExprAlias
  lookupTypeAlias = lift . lookupTypeAlias

  withFixScope = liftWithReaderT . withFixScope
  isInFixScope = lift . isInFixScope

liftWithReaderT :: Monad m => (m a -> m a) -> ReaderT r m a -> ReaderT r m a
liftWithReaderT f act = do
  env <- ask
  lift $ f $ runReaderT act env

