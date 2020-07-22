{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Address.DerivationSpec
    ( spec
    ) where

import Prelude

import Cardano.Address.Derivation
    ( Depth (..)
    , DerivationType (..)
    , Index
    , XPrv
    , sign
    , toXPub
    , verify
    , xprvChainCode
    , xprvFromBytes
    , xprvToBytes
    , xpubChainCode
    , xpubFromBytes
    , xpubToBytes
    )
import Data.ByteArray
    ( ScrubbedBytes )
import Data.ByteString
    ( ByteString )
import Data.String
    ( IsString (..) )
import Test.Hspec
    ( Spec, describe, it )
import Test.Hspec.QuickCheck
    ( prop )
import Test.QuickCheck
    ( Arbitrary (..), Property, expectFailure, property, (.&&.), (===) )

import Test.Arbitrary
    ()

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

    describe "XPub / XPrv properties" $ do
        prop "roundtripping: xpubToBytes . xpubFromBytes" $
            prop_roundtripBytes xpubToBytes xpubFromBytes
        prop "roundtripping: xprvToBytes . xprvFromBytes" $
            prop_roundtripBytes xprvToBytes xprvFromBytes
        prop "forall xprv. xprvChainCode xprv == xpubChainCode (toXPub xprv)"
            prop_chainCodeInvariance
        prop "forall xprv msg. 'verify' ('toXPub' xprv) msg ('sign' xprv msg) == 'True'"
            prop_publicKeySignature

{-------------------------------------------------------------------------------
                               Properties
-------------------------------------------------------------------------------}

prop_publicKeySignature
    :: XPrv
    -> ScrubbedBytes
    -> Property
prop_publicKeySignature xprv msg =
    verify (toXPub xprv) msg (sign xprv msg) === True

prop_chainCodeInvariance
    :: XPrv
    -> Property
prop_chainCodeInvariance xprv =
    xprvChainCode xprv === xpubChainCode (toXPub xprv)

prop_roundtripBytes
    :: (Eq a, Show a)
    => (a -> ByteString)
    -> (ByteString -> Maybe a)
    -> a
    -> Property
prop_roundtripBytes encode decode a =
    decode (encode a) === pure a

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
    (toEnum . fromEnum) ix === ix .&&. (toEnum . fromEnum) ix === ix

prop_roundtripEnumIndexSoft :: Index 'Soft 'AddressK -> Property
prop_roundtripEnumIndexSoft ix =
    (toEnum . fromEnum) ix === ix .&&. (toEnum . fromEnum) ix === ix

{-------------------------------------------------------------------------------
                             Arbitrary Instances
-------------------------------------------------------------------------------}

instance Arbitrary ScrubbedBytes where
    arbitrary = fromString <$> arbitrary
