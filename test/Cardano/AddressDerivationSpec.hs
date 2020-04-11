{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.AddressDerivationSpec
    ( spec
    ) where

import Prelude

import Cardano.AddressDerivation
    ( Depth (..), DerivationType (..), Index, getIndex )
import Cardano.Mnemonic
    ( ConsistentEntropy
    , EntropySize
    , Mnemonic (..)
    , SomeMnemonic (..)
    , ValidEntropySize
    , ValidChecksumSize
    , entropyToMnemonic
    , Entropy
    , mkEntropy
    )
import GHC.Stack
    ( HasCallStack )
import Data.ByteString
    ( ByteString )
import GHC.TypeLits
    ( natVal )
import Data.Proxy
    ( Proxy (..) )
import Test.Hspec
    ( Spec, describe, it)
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , Property
    , arbitraryBoundedEnum
    , expectFailure
    , property
    , vector
    , (.&&.)
    , (===)
    )

import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS

spec :: Spec
spec = describe "Checking auxiliary address derivations types" $ do
    describe "Bounded / Enum relationship" $ do
        it "The calls Index.succ maxBound should result in a runtime err (hard)"
            prop_succMaxBoundHardIx
        it "The calls Index.pred minBound should result in a runtime err (hard)"
            prop_predMinBoundHardIx
        it "The calls Index.succ maxBound should result in a runtime err (soft)"
            prop_succMaxBoundSoftIx
        it "The calls Index.pred minBound should result in a runtime err (soft)"
            prop_predMinBoundSoftIx

    describe "Enum Roundtrip" $ do
        it "Index @'Hardened _" (property prop_roundtripEnumIndexHard)
        it "Index @'Soft _" (property prop_roundtripEnumIndexSoft)

{-------------------------------------------------------------------------------
                               Properties
-------------------------------------------------------------------------------}

prop_succMaxBoundHardIx :: Property
prop_succMaxBoundHardIx = expectFailure $
    property $ succ (maxBound @(Index 'Hardened _)) `seq` ()

prop_predMinBoundHardIx :: Property
prop_predMinBoundHardIx = expectFailure $
    property $ pred (minBound @(Index 'Hardened _)) `seq` ()

prop_succMaxBoundSoftIx :: Property
prop_succMaxBoundSoftIx = expectFailure $
    property $ succ (maxBound @(Index 'Soft _)) `seq` ()

prop_predMinBoundSoftIx :: Property
prop_predMinBoundSoftIx = expectFailure $
    property $ pred (minBound @(Index 'Soft _)) `seq` ()

prop_roundtripEnumIndexHard :: Index 'WholeDomain 'AccountK -> Property
prop_roundtripEnumIndexHard ix =
    (toEnum . fromEnum) ix === ix .&&. (toEnum . fromEnum . getIndex) ix === ix

prop_roundtripEnumIndexSoft :: Index 'Soft 'AddressK -> Property
prop_roundtripEnumIndexSoft ix =
    (toEnum . fromEnum) ix === ix .&&. (toEnum . fromEnum . getIndex) ix === ix

{-------------------------------------------------------------------------------
                             Arbitrary Instances
-------------------------------------------------------------------------------}

instance Arbitrary (Index 'Soft 'AddressK) where
    shrink _ = []
    arbitrary = arbitraryBoundedEnum

instance Arbitrary (Index 'Hardened 'AccountK) where
    shrink _ = []
    arbitrary = arbitraryBoundedEnum

instance Arbitrary (Index 'Hardened 'AddressK) where
    shrink _ = []
    arbitrary = arbitraryBoundedEnum

instance Arbitrary (Index 'WholeDomain 'AddressK) where
    shrink _ = []
    arbitrary = arbitraryBoundedEnum

instance Arbitrary (Index 'WholeDomain 'AccountK) where
    shrink _ = []
    arbitrary = arbitraryBoundedEnum

instance Arbitrary SomeMnemonic where
    arbitrary = SomeMnemonic <$> genMnemonic @12

-- | Generates an arbitrary mnemonic of a size according to the type parameter.
--
-- E.g:
-- >>> arbitrary = SomeMnemonic <$> genMnemonic @12
genMnemonic
    :: forall mw ent csz.
     ( ConsistentEntropy ent mw csz
     , EntropySize mw ~ ent
     )
    => Gen (Mnemonic mw)
genMnemonic = do
        let n = fromIntegral (natVal $ Proxy @(EntropySize mw)) `div` 8
        bytes <- BS.pack <$> vector n
        let ent = unsafeMkEntropy @(EntropySize mw) bytes
        return $ entropyToMnemonic ent

unsafeMkEntropy
    :: forall ent csz.
        ( HasCallStack
        , ValidEntropySize ent
        , ValidChecksumSize ent csz
        )
    => ByteString
    -> Entropy ent
unsafeMkEntropy = either (error . show) id . mkEntropy . BA.convert
