-- | Haskell bidings to Xen hypervisor interface. There are three interface levels
-- in this library:
--
--   * Low-level interface. "System.Xen.Low". It just provides bindings to c-calls.
--
--   * Mid-level interface. "System.Xen.Mid". Contains helper functions and allow to
--     use your favorite `Monad`.
--
--   * High-level interface. "System.Xen.High". Contains `Xen` monad and provides a
--     safe way to run any `Xen` computation.
--
-- Last one is also re-exported by current module and intend for common usage.
-- Usage example:
--
-- > module Main (main) where
-- >
-- > import System.Xen (runXenT, domainGetInfo)
-- >
-- > main :: IO ()
-- > main = print =<< runXenT domainGetInfo

module System.Xen
    (
    -- * Errors
      XcHandleOpenError(..)
    , InvalidDomainShutdownReason(..)
    , DomainGetInfoError(..)
    -- * Domain info
    , DomId(..)
    , DomainFlag(..)
    , DomainShutdownReason(..)
    , DomainInfo(..)
    -- * High-level API
    , XenT
    , Xen
    , domainGetInfo
    , domainPause
    , domainUnpause
    , runXenT
    ) where

import System.Xen.Errors (XcHandleOpenError(..), InvalidDomainShutdownReason(..),
                          DomainGetInfoError(..))
import System.Xen.High (XenT, Xen, domainGetInfo, domainPause, domainUnpause,
                        runXenT)
import System.Xen.Types (DomId(..), DomainFlag(..), DomainShutdownReason(..),
                         DomainInfo(..))
