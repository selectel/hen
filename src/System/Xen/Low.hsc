{-# LANGUAGE ForeignFunctionInterface #-}

-- | Low-level interface to @XenCtrl@. Each function defined in this module is
-- a ffi call to corresponding c function.

module System.Xen.Low
    ( xc_interface_open
    , xc_interface_close
    , xc_domain_getinfo
    ) where

#include <xenctrl.h>

import Foreign.C (CInt(..), CUInt(..))
#if XEN_SYSCTL_INTERFACE_VERSION == 8
import Foreign.C (CIntPtr(..))
#endif
import Foreign.Ptr (Ptr)

import System.Xen.Types (XcHandle(..), DomainInfo(..), DomId(..))

-- | This function opens the handle to the hypervisor interface. Each successful call
--  to this function should have a corresponding call to 'xc_interface_close'.
foreign import ccall unsafe "xc_interface_open"
-- There are some changes in xen 4 in comparsion with 3, 'XcHandle' is not a
-- file descriptor, but a pointer to special structure.
#if XEN_SYSCTL_INTERFACE_VERSION == 8
    xc_interface_open :: CInt  -- ^ Logger, @NULL@ if stderr
                      -> CInt  -- ^ Domain builder logger
                      -> CInt  -- ^ Open flags
                      -> IO XcHandle
#elif XEN_SYSCTL_INTERFACE_VERSION == 6
    xc_interface_open :: IO XcHandle
#endif

-- | This function closes an open hypervisor interface. This function can fail if the
-- handle does not represent an open interface or if there were problems closing the
-- interface. In the latter case the interface is still closed.
foreign import ccall unsafe "xc_interface_close"
    xc_interface_close :: XcHandle -> IO CInt

-- | This function will return information about one or more domains. It is
-- designed to iterate over the list of domains. If a single domain is
-- requested, this function will return the next domain in the list - if
-- one exists. It is, therefore, important in this case to make sure the
-- domain requested was the one returned.
foreign import ccall unsafe "xenctrl.h xc_domain_getinfo"
    xc_domain_getinfo :: XcHandle  -- ^ Handle to the open hypervisor interface
                      -> DomId -- ^ First domain to enumerate from.
                      -> CUInt -- ^ The number of requested domains
                      -> Ptr DomainInfo  -- ^ Pointer to the structure that will
                                         -- contain the information for
                                         -- enumerated domains
                      -> IO CInt  -- ^ Number of domains enumerated, -1 on error