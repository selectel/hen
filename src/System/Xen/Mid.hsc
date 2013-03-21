{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Mid-level interface to @XenCtrl@. Functions that provided by this module are
-- version-independent from @Xen@ and raise real exceptions instead of confusing
-- error codes and @errno@.

module System.Xen.Mid
    ( interfaceOpen
    , interfaceClose
    , domainGetInfo
    ) where

#include <xenctrl.h>

import Control.Monad (void, when, forM)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Storable (peekElemOff, sizeOf)

import Control.Exception.Lifted (throwIO)
import Control.Monad.Base (MonadBase(liftBase))

import System.Xen.Errors (DomainGetInfoError(..), XcHandleOpenError(..), getErrno)
import System.Xen.Low (xc_interface_open, xc_interface_close, xc_domain_getinfo)
import System.Xen.Types (XcHandle(..), DomId(..), DomainInfo)

-- | Open the connection to the hypervisor interface, can fail with
-- 'System.Xen.Errors.XcHandleOpenError'.
interfaceOpen :: MonadBase IO m => m XcHandle
interfaceOpen = liftBase $ do
#if XEN_SYSCTL_INTERFACE_VERSION == 8
    i@(XcHandle ptr) <- xc_interface_open 0 0 0
    when (ptr `elem` [-1, 0]) $ getErrno >>= throwIO . XcHandleOpenError
#elif XEN_SYSCTL_INTERFACE_VERSION == 6
    i@(XcHandle h) <- xc_interface_open
    when (h == -1) $ getErrno >>= throwIO . XcHandleOpenError
#endif
    return i

-- | Close an open hypervisor interface, ignores all possible errors but all the
-- same can fail with segfault or sutin.
interfaceClose :: MonadBase IO m => XcHandle -> m ()
interfaceClose = void . liftBase . xc_interface_close

-- | Returns a list of currently runing domains, 1024 maximum, can fail with
-- 'System.Xen.Errors.InvalidDomainShutdownReason' and
-- 'System.Xen.Errors.DomainGetInfoError'.
domainGetInfo :: MonadBase IO m => XcHandle -> m [DomainInfo]
domainGetInfo handle = liftBase $ allocaBytes size $ \ptr -> do
     wrote <- fmap fromIntegral $ xc_domain_getinfo handle (dom0) count ptr
     when (wrote == -1) $ getErrno >>= throwIO . DomainGetInfoError
     forM [0 .. wrote - 1] $ peekElemOff ptr
  where
    dom0 = DomId 0
    count :: Num a => a
    count = 1024
    size = count * sizeOf (undefined :: DomainInfo)
