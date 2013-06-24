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
    -- * Domain pause
    , domainPause
    , domainUnpause
    ) where

import System.Xen.High.Internal (XenT, Xen, MonadXen(withXenHandle), runXenT)
import System.Xen.Types (DomainInfo, DomId)
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
