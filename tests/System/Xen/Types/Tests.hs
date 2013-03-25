{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module System.Xen.Types.Tests (tests) where

import Control.Applicative ((<$>), (<*>))
import Data.Word (Word32)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Storable (Storable(..))

import Data.BitSet (BitSet, fromList, member)
import Data.UUID (UUID, fromWords)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Property, Arbitrary(..), elements)
import Test.QuickCheck.Monadic (monadicIO, assert, run)

import System.Xen.Types (DomId(..), DomainShutdownReason(..), DomainInfo(..),
                         DomainFlag(..))

instance Arbitrary DomainShutdownReason where
    arbitrary = elements [ DomainShutdownReasonPoweroff
                         , DomainShutdownReasonReboot
                         , DomainShutdownReasonSuspend
                         , DomainShutdownReasonCrash
                         , DomainShutdownReasonWatchdog
                         ]

instance Arbitrary DomainFlag where
    arbitrary = elements [ DomainFlagDying
                         , DomainFlagCrashed
                         , DomainFlagShutdown
                         , DomainFlagPaused
                         , DomainFlagBlocked
                         , DomainFlagRunning
                         , DomainFlagHVM
                         , DomainFlagDebugged
                         ]

deriving instance Arbitrary DomId

instance Arbitrary UUID where
    arbitrary = fromWords <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Arbitrary a, Enum a) => Arbitrary (BitSet a) where
    arbitrary = fmap fromList arbitrary

instance Arbitrary DomainInfo where
    arbitrary = do
        domainInfoId <- arbitrary
        domainInfoSsidRef <- arbitrary
        domainInfoFlags <- arbitrary
        domainInfoShutdownReason <- case DomainFlagShutdown `member` domainInfoFlags of
            True -> fmap Just arbitrary
            False -> return Nothing
        domainInfoNumberOfPages <- arbitrary
        domainInfoNumberOfSharedPages :: Word32 <- arbitrary
        domainInfoSharedInfoFrame <- arbitrary
        domainInfoCpuTime <- arbitrary
        domainInfoMaxMemKb <- arbitrary
        domainInfoNubmerOfOnlineVcpus <- arbitrary
        domainInfoMaxVcpuId <- arbitrary
        domainInfoDomHandle <- arbitrary
        domainInfoCpuPool :: Word32 <- arbitrary
        return $ DomainInfo { .. }


testStorable :: (Arbitrary a, Storable a, Eq a) => a -> Property
testStorable storable = monadicIO $ do
    peeked <- run $ do
        allocaBytes size $ \ptr -> do
            poke ptr storable
            peek ptr
    assert $ storable == peeked
  where
    size = sizeOf storable

tests :: Test
tests = testGroup "System.Xen.Types.Tests"
    [ testProperty "Storable instance for DomainShutdownReason" (testStorable :: DomainShutdownReason -> Property)
    , testProperty "Storable instance for DomainInfo" (testStorable :: DomainInfo -> Property)
    ]
