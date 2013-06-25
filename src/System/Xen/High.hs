-- | High-level interface to @XenCtrl@. Contains `Xen` monad and provides a safe way
-- to run any `Xen` computation.
module System.Xen.High
    (
    -- * Monad stuff
      XenT
    , Xen
    , runXenT
    -- * Domain
    , domainGetInfo
    -- ** Domain pause
    , domainPause
    , domainUnpause
    -- ** Domain powerstate
    , domainShutdown
    , domainDestroy
    ) where

import System.Xen.High.Internal (XenT, Xen, MonadXen(withXenHandle), runXenT)
import System.Xen.Types (DomainInfo, DomId, DomainShutdownReason)
import qualified System.Xen.Mid as Mid

-- | Returns a lift of domains, this function can fail with
-- 'System.Xen.Errors.InvalidDomainShutdownReason' and
-- 'System.Xen.Errors.DomainGetInfoError'.
domainGetInfo :: MonadXen m => m [DomainInfo]
domainGetInfo = withXenHandle Mid.domainGetInfo

-- | Pause domain. A paused domain still exists in memory
-- however it does not receive any timeslices from the hypervisor.
domainPause :: MonadXen m => DomId -> m Bool
domainPause = withXenHandle . Mid.domainPause

-- | Unpause a domain. The domain should have been previously paused.
domainUnpause :: MonadXen m => DomId -> m Bool
domainUnpause = withXenHandle . Mid.domainUnpause

-- | Shutdown domain. This is intended for use in fully-virtualized domains where
-- this operation is analogous to the sched_op operations in a paravirtualized domain.
domainShutdown :: MonadXen m => DomId -> DomainShutdownReason -> m Bool
domainShutdown domid reason = withXenHandle $ Mid.domainShutdown domid reason

-- | Destroy a domain.  Destroying a domain removes the domain completely from memory.
-- This function should be called after 'domainShutdown' to free up the domain resources.
domainDestroy :: MonadXen m => DomId -> m Bool
domainDestroy = withXenHandle . Mid.domainDestroy
