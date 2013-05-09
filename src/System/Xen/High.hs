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
import Control.Exception (SomeException)

import Control.Monad.Exception (MonadException, try, bracket)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Trans (MonadIO(liftIO))

import System.Xen.Types (XcHandle, DomainInfo)
import qualified System.Xen.Mid as Mid

-- | This is a special monad for operations with @XenCtrl@, it's a wrapper around
-- 'ReaderT' transformer and it controls the connection to the hypervisor.
newtype Xen a = Xen (ReaderT XcHandle IO a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadException)

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
runXen :: (MonadException m, MonadIO m, Functor m) => Xen a -> m (Either SomeException a)
runXen (Xen f) = try $ withNewHandle $ liftIO . runReaderT f
  where
    withNewHandle = bracket Mid.interfaceOpen Mid.interfaceClose
