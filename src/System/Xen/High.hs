{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}

-- | High-level interface to @XenCtrl@. Contains `Xen` monad and provides a safe way
-- to run any `Xen` computation.
module System.Xen.High
    ( Xen
    , domainGetInfo
    , withXenHandle
    , runXen
    ) where

import Control.Applicative (Applicative)
import Control.Exception.Lifted (SomeException, bracket, try)
import Control.Monad (liftM)

import Control.Monad.Base (MonadBase(..))
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Trans.Control (MonadBaseControl(..))

import System.Xen.Types (XcHandle, DomainInfo)
import qualified System.Xen.Mid as Mid

-- | This is a special monad for operations with @XenCtrl@, it's a wrapper around
-- 'ReaderT' transformer and it controls the connection to the hypervisor.
-- Because 'Xen' has instances of and 'MonadBase' and 'MonadBaseControl' over 'IO',
-- you can use any functions of @lifted-base@ library, or any 'IO' with 'liftBase'.
newtype Xen a = Xen { unXen :: ReaderT XcHandle IO a }
    deriving (Functor, Applicative, Monad, MonadBase IO)

instance MonadBaseControl IO Xen where
    newtype StM Xen a = StXen { unStXen :: StM (ReaderT XcHandle IO) a }
    liftBaseWith f = Xen $ liftBaseWith $ \runInIO ->
        f $ liftM StXen . runInIO . unXen
    restoreM = Xen . restoreM . unStXen

-- | Returns a lift of domains, this function can fail with
-- 'System.Xen.Errors.InvalidDomainShutdownReason' and
-- 'System.Xen.Errors.DomainGetInfoError'.
domainGetInfo :: Xen [DomainInfo]
domainGetInfo = withXenHandle Mid.domainGetInfo

-- | Helper function for creating high-level interface functions from mid-level.
-- Generally high-level function is just @highLevel = withXenHandle midLevel@.
withXenHandle :: (XcHandle -> Xen a) -> Xen a
withXenHandle f = f =<< Xen ask

-- | Open new connection to the hypervisor, run any 'Xen' action and close
-- connection if nessesary. This function can fail with @Either SomeException@ with
-- 'System.Xen.Errors.XcHandleOpenError' and any error of providing 'Xen' action.
runXen :: MonadBaseControl IO m => Xen a -> m (Either SomeException a)
runXen (Xen f) = try $ withNewHandle $ liftBase . runReaderT f
  where
    withNewHandle = bracket Mid.interfaceOpen Mid.interfaceClose
