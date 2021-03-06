{-# LANGUAGE ForeignFunctionInterface #-}

-- | Mid-level interface to @XenCtrl@. Functions that provided by this module are
-- version-independent from @Xen@ and raise real exceptions instead of confusing
-- error codes and @errno@.

module System.Xen.Mid
    (
    -- * Interface
      interfaceOpen
    , interfaceClose
    -- * Domain
    , domainGetInfo
    -- ** Domain pause
    , domainPause
    , domainUnpause
    -- ** Domain powerstate
    , domainShutdown
    , domainDestroy
    ) where

#include <xenctrl.h>

import Prelude hiding (sequence)

import Control.Applicative (Alternative(..), pure)
import Control.Monad (void, when)
import Data.Traversable (Traversable(sequenceA))
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Storable (peekElemOff, peek, poke, sizeOf)
import Foreign.Ptr (castPtr)

import Control.Monad.Catch (throwM)
import Control.Monad.Trans (MonadIO(liftIO))

import System.Xen.Errors (DomainGetInfoError(..), XcHandleOpenError(..), getErrno)
import System.Xen.Types (XcHandle(..), DomId(..), DomainShutdownReason, DomainInfo)
import qualified System.Xen.Low as Low

-- | Open the connection to the hypervisor interface, can fail with
-- 'System.Xen.Errors.XcHandleOpenError'.
interfaceOpen :: MonadIO m => m XcHandle
interfaceOpen = liftIO $ do
#if XEN_SYSCTL_INTERFACE_VERSION == 8
    i@(XcHandle ptr) <- Low.xc_interface_open 0 0 0
    when (ptr `elem` [-1, 0]) $ getErrno >>= throwM . XcHandleOpenError
#elif XEN_SYSCTL_INTERFACE_VERSION == 6
    i@(XcHandle h) <- Low.xc_interface_open
    when (h == -1) $ getErrno >>= throwM . XcHandleOpenError
#endif
    return i

-- | Close an open hypervisor interface, ignores all possible errors but all the
-- same can fail with segfault or sutin.
interfaceClose :: (MonadIO m, Functor m) => XcHandle -> m ()
interfaceClose = void . liftIO . Low.xc_interface_close

-- | Returns a list of currently runing domains, 1024 maximum, can fail with
-- 'System.Xen.Errors.InvalidDomainShutdownReason' and
-- 'System.Xen.Errors.DomainGetInfoError'.
domainGetInfo :: (MonadIO m, Alternative t, Traversable t) => XcHandle -> m (t DomainInfo)
domainGetInfo handle = liftIO $ allocaBytes size $ \ptr -> do
     wrote <- fmap fromIntegral $ Low.xc_domain_getinfo handle (dom0) count ptr
     when (wrote == -1) $ getErrno >>= throwM . DomainGetInfoError
     sequenceA $ generateA wrote $ peekElemOff ptr
  where
    dom0 = DomId 0
    count :: Num a => a
    count = 1024
    size = count * sizeOf (undefined :: DomainInfo)
    generateA c = go empty c c
      where
        go t 0 _ _ = t
        go t n l a = n `seq` go (pure (a (l - n)) <|> t) l (n - 1) a

-- | Pause domain. A paused domain still exists in memory
-- however it does not receive any timeslices from the hypervisor.
domainPause :: MonadIO m => DomId -> XcHandle -> m Bool
domainPause domid handle = liftIO $ fmap (== 0) $ Low.xc_domain_pause handle domid

-- | Unpause a domain. The domain should have been previously paused.
domainUnpause :: MonadIO m => DomId -> XcHandle -> m Bool
domainUnpause domid handle = liftIO $ fmap (== 0) $ Low.xc_domain_unpause handle domid

-- | Shutdown domain. This is intended for use in fully-virtualized domains where
-- this operation is analogous to the sched_op operations in a paravirtualized domain.
domainShutdown :: MonadIO m => DomId -> DomainShutdownReason -> XcHandle -> m Bool
domainShutdown domid reason handle = liftIO $ fmap (== 0) $ allocaBytes size $ \ptr -> do
    poke ptr reason
    intReason <- peek $ castPtr ptr
    Low.xc_domain_shutdown handle domid intReason
  where
    size = sizeOf (undefined :: DomainShutdownReason)

-- | Destroy a domain.  Destroying a domain removes the domain completely from memory.
-- This function should be called after 'domainShutdown' to free up the domain resources.
domainDestroy :: MonadIO m => DomId -> XcHandle -> m Bool
domainDestroy domid handle = liftIO $ fmap (== 0) $ Low.xc_domain_destroy handle domid
