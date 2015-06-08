{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | This module provides every special exception that can be raised in Mid and
-- High-level interfaces.

module System.Xen.Errors
    ( XcHandleOpenError(..)
    , InvalidDomainShutdownReason(..)
    , DomainGetInfoError(..)
    , getErrno
    ) where

import Control.Exception (Exception)
import Data.Orphans ()
import Data.Typeable (Typeable)
import Foreign.C (CInt)
import Foreign.C.Error (Errno(..))
import qualified Foreign.C.Error as Error

import Control.Monad.Trans (MonadIO(liftIO))

deriving instance Ord Errno
deriving instance Show Errno

-- | This error can be raised if handle can not be opened, insufficient rights
-- for example.
data XcHandleOpenError = XcHandleOpenError Errno
    deriving (Eq, Ord, Show, Typeable)

instance Exception XcHandleOpenError

-- | This error can be raised if peecked value of
-- 'System.Xen.Types.DomainShutdownReason' is not expected.
data InvalidDomainShutdownReason = InvalidDomainShutdownReason
    CInt  -- ^ Peeked value
    deriving (Eq, Ord, Show, Typeable)

instance Exception InvalidDomainShutdownReason

-- | This error can be raised if any error occured during receiving the list,
-- for example: try to to fetch a list in domU.
data DomainGetInfoError = DomainGetInfoError Errno
    deriving (Eq, Ord, Show, Typeable)

instance Exception DomainGetInfoError

-- | Generalized version of 'Foreign.C.Error.getErrno'
getErrno :: MonadIO m => m Errno
getErrno = liftIO Error.getErrno
