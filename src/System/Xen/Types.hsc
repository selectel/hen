{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

-- | Types for working with 'XenCtrl' data and accoring 'Storable' instances.
module System.Xen.Types
    ( XcHandle(..)
    , DomId(..)
    , DomainFlag(..)
    , DomainShutdownReason(..)
    , DomainInfo(..)
    ) where

#include <xenctrl.h>
#include <xen/sched.h>

#let alignment t = "%lu", (unsigned long) offsetof(struct {char x__; t (y__); }, y__)

import Prelude hiding (elem, foldl)

import Control.Applicative ((<$>))
import Data.Bits (testBit, bit, (.|.))
import Data.Maybe (catMaybes)
import Data.Word (Word32, Word64)
import Foreign.C (CInt(..), CUInt(..))
#if XEN_SYSCTL_INTERFACE_VERSION == 8
import Foreign.C (CIntPtr(..))
#endif
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable(..))

import Control.Monad.Catch (throwM)
import Data.UUID (UUID)
import Data.BitSet (BitSet)
import qualified Data.BitSet as BitSet

import System.Xen.Errors (InvalidDomainShutdownReason(..))

-- | Entry point of the hypervisor interface connection, it's a file descriptor
-- in xen 3 and pointer to corresponging structure in xen 4.
#if XEN_SYSCTL_INTERFACE_VERSION == 8
newtype XcHandle = XcHandle CIntPtr
#elif XEN_SYSCTL_INTERFACE_VERSION == 6
newtype XcHandle = XcHandle CInt
#endif
    deriving (Eq, Ord, Show, Storable)

-- | Domain id, wrapper around 'Word32'.
newtype DomId = DomId { unDomId :: Word32 }
    deriving (Eq, Ord, Show, Read, Storable)

-- | Domain flags. It's translated from xc_dominfo structure, so it's possible to
-- be mutual exclusion flags in one domain, e.g. 'DomainFlagShutdown' and
-- 'DomainFlagRunning'.
data DomainFlag = DomainFlagDying
                | DomainFlagCrashed
                | DomainFlagShutdown
                | DomainFlagPaused
                | DomainFlagBlocked
                | DomainFlagRunning
                | DomainFlagHVM
                | DomainFlagDebugged
    deriving (Enum, Eq, Ord, Show, Read)

-- | Domain shutdown reason it's only meaningful if domain has 'DomainFlagShutdown'
-- flag.
data DomainShutdownReason = DomainShutdownReasonPoweroff
                          | DomainShutdownReasonReboot
                          | DomainShutdownReasonSuspend
                          | DomainShutdownReasonCrash
                          | DomainShutdownReasonWatchdog
    deriving (Eq, Ord, Show, Read)

-- | Information about a single domain.
data DomainInfo = DomainInfo
    { domainInfoId                  :: {-# UNPACK #-} !DomId
    , domainInfoSsidRef             :: {-# UNPACK #-} !Word32
    , domainInfoFlags               :: !(BitSet DomainFlag)
    , domainInfoShutdownReason      :: !(Maybe DomainShutdownReason)
    , domainInfoNumberOfPages       :: {-# UNPACK #-} !Word32
#if XEN_SYSCTL_INTERFACE_VERSION == 8
    , domainInfoNumberOfSharedPages :: {-# UNPACK #-} !Word32
#endif
    , domainInfoSharedInfoFrame     :: {-# UNPACK #-} !Word32
    , domainInfoCpuTime             :: {-# UNPACK #-} !Word64
    , domainInfoMaxMemKb            :: {-# UNPACK #-} !Word32
    , domainInfoNubmerOfOnlineVcpus :: {-# UNPACK #-} !Word32
    , domainInfoMaxVcpuId           :: {-# UNPACK #-} !Word32
    , domainInfoDomHandle           :: {-# UNPACK #-} !UUID
#if XEN_SYSCTL_INTERFACE_VERSION == 8
    , domainInfoCpuPool             :: {-# UNPACK #-} !Word32
#endif
    } deriving (Eq, Ord, Show, Read)

-- | Constats used in this instance defined in <xen/sched.h>.
instance Storable DomainShutdownReason where
    sizeOf _ = sizeOf (undefined :: CInt)
    alignment _ = alignment (undefined :: CInt)
    peek ptr = peek (castPtr ptr :: Ptr CInt) >>= \i -> case i of
        #{const SHUTDOWN_poweroff} -> return DomainShutdownReasonPoweroff
        #{const SHUTDOWN_reboot}   -> return DomainShutdownReasonReboot
        #{const SHUTDOWN_suspend}  -> return DomainShutdownReasonSuspend
        #{const SHUTDOWN_crash}    -> return DomainShutdownReasonCrash
        #{const SHUTDOWN_watchdog} -> return DomainShutdownReasonWatchdog
        invalid           -> throwM $ InvalidDomainShutdownReason invalid
    poke ptr a = poke (castPtr ptr :: Ptr CInt) $ case a of
        DomainShutdownReasonPoweroff -> #{const SHUTDOWN_poweroff}
        DomainShutdownReasonReboot   -> #{const SHUTDOWN_reboot}
        DomainShutdownReasonSuspend  -> #{const SHUTDOWN_suspend}
        DomainShutdownReasonCrash    -> #{const SHUTDOWN_crash}
        DomainShutdownReasonWatchdog -> #{const SHUTDOWN_watchdog}

instance Storable DomainInfo where
    sizeOf _ = #{size xc_dominfo_t}
    alignment _ = #{alignment xc_dominfo_t}
    peek ptr = do
        domainInfoId                  <- #{peek xc_dominfo_t, domid} ptr
        domainInfoSsidRef             <- #{peek xc_dominfo_t, ssidref} ptr
        domainInfoFlags               <- do
            b :: CUInt <- peekByteOff ptr $
                sizeOf domainInfoId + sizeOf domainInfoSsidRef
            let maybeBit n v = if testBit b n then Just v else Nothing
            return $ BitSet.fromList $ catMaybes
               [ maybeBit 0 DomainFlagDying
               , maybeBit 1 DomainFlagCrashed
               , maybeBit 2 DomainFlagShutdown
               , maybeBit 3 DomainFlagPaused
               , maybeBit 4 DomainFlagBlocked
               , maybeBit 5 DomainFlagRunning
               , maybeBit 6 DomainFlagHVM
               , maybeBit 7 DomainFlagDebugged
               ]
        domainInfoShutdownReason      <-
            if DomainFlagShutdown `BitSet.member` domainInfoFlags
            then Just <$> #{peek xc_dominfo_t, shutdown_reason} ptr
            else return Nothing
        domainInfoNumberOfPages       <- #{peek xc_dominfo_t, nr_pages} ptr
#if XEN_SYSCTL_INTERFACE_VERSION == 8
        domainInfoNumberOfSharedPages <- #{peek xc_dominfo_t, nr_shared_pages} ptr
#endif
        domainInfoSharedInfoFrame     <- #{peek xc_dominfo_t, shared_info_frame} ptr
        domainInfoCpuTime             <- #{peek xc_dominfo_t, cpu_time} ptr
        domainInfoMaxMemKb            <- #{peek xc_dominfo_t, max_memkb} ptr
        domainInfoNubmerOfOnlineVcpus <- #{peek xc_dominfo_t, nr_online_vcpus} ptr
        domainInfoMaxVcpuId           <- #{peek xc_dominfo_t, max_vcpu_id} ptr
        domainInfoDomHandle           <- #{peek xc_dominfo_t, handle} ptr
#if XEN_SYSCTL_INTERFACE_VERSION == 8
        domainInfoCpuPool             <- #{peek xc_dominfo_t, cpupool} ptr
#endif
        return $ DomainInfo { .. }
    poke ptr DomainInfo { .. } = do
        #{poke xc_dominfo_t, domid} ptr domainInfoId
        #{poke xc_dominfo_t, ssidref} ptr domainInfoSsidRef
        let off = sizeOf domainInfoId + sizeOf domainInfoSsidRef
        let flags :: CUInt = BitSet.foldl' (\a b -> a .|. bit (fromEnum b)) 0 domainInfoFlags
        pokeByteOff ptr off flags
        case domainInfoShutdownReason of
            Nothing -> return ()
            Just reason -> #{poke xc_dominfo_t, shutdown_reason} ptr reason
        #{poke xc_dominfo_t, nr_pages} ptr domainInfoNumberOfPages
#if XEN_SYSCTL_INTERFACE_VERSION == 8
        #{poke xc_dominfo_t, nr_shared_pages} ptr domainInfoNumberOfSharedPages
#endif
        #{poke xc_dominfo_t, shared_info_frame} ptr domainInfoSharedInfoFrame
        #{poke xc_dominfo_t, cpu_time} ptr domainInfoCpuTime
        #{poke xc_dominfo_t, max_memkb} ptr domainInfoMaxMemKb
        #{poke xc_dominfo_t, nr_online_vcpus} ptr domainInfoNubmerOfOnlineVcpus
        #{poke xc_dominfo_t, max_vcpu_id} ptr domainInfoMaxVcpuId
        #{poke xc_dominfo_t, handle} ptr domainInfoDomHandle
#if XEN_SYSCTL_INTERFACE_VERSION == 8
        #{poke xc_dominfo_t, cpupool} ptr domainInfoCpuPool
#endif
