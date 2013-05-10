{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | High-level interface to @XenCtrl@. Contains `Xen` monad and provides a safe way
-- to run any `Xen` computation.
module System.Xen.High
    ( XenT
    , Xen
    , domainGetInfo
    , withXenHandle
    , runXen
    ) where

import Control.Applicative (Applicative)
import Control.Exception (SomeException)

import Control.Monad.Exception (MonadException, try, bracket)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Trans (MonadIO, MonadTrans(lift))

import System.Xen.Types (XcHandle, DomainInfo)
import qualified System.Xen.Mid as Mid

newtype XenT m a = XenT (ReaderT XcHandle m a)
    deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, MonadException)

class (Functor m, MonadIO m, MonadException m) => MonadXen m where
    -- | Open new connection to the hypervisor, run any @Xen@ action and close
    -- connection if nessesary. This function can fail with @Either SomeException@ with
    -- 'System.Xen.Errors.XcHandleOpenError' and any error of providing @Xen@ action.
    runXen :: m a -> m (Either SomeException a)
    -- | Helper function for creating high-level interface functions from mid-level.
    -- Generally high-level function is just @highLevel = withXenHandle midLevel@.
    withXenHandle :: (XcHandle -> m a) -> m a

instance (Functor m, MonadIO m, MonadException m) => MonadXen (XenT m) where
    runXen (XenT f) = try $ withNewHandle $ lift . runReaderT f
      where
        withNewHandle = bracket Mid.interfaceOpen Mid.interfaceClose
    withXenHandle f = f =<< XenT ask

type Xen = XenT IO

-- | Returns a lift of domains, this function can fail with
-- 'System.Xen.Errors.InvalidDomainShutdownReason' and
-- 'System.Xen.Errors.DomainGetInfoError'.
domainGetInfo :: MonadXen m => m [DomainInfo]
domainGetInfo = withXenHandle Mid.domainGetInfo
