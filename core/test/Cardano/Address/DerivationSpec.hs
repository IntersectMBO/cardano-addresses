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
    , Indexed (..)
    , XPrv
    , nextIndex
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
    ( Spec, describe, it, shouldBe )
import Test.Hspec.QuickCheck
    ( prop )
import Test.QuickCheck
    ( Arbitrary (..), Property, property, (===) )

import Test.Arbitrary
    ()

spec :: Spec
spec = describe "Checking auxiliary address derivations types" $ do
    describe "Bounded / Indexed relationship" $ do
        it "nextIndex maxBound for Hardened indices should result in failure" $
            nextIndex (maxBound @(Index 'Hardened _)) `shouldBe` Nothing
        it "nextIndex maxBound for Soft indices should result in failure" $
            nextIndex (maxBound @(Index 'Soft _)) `shouldBe` Nothing

    describe "Indexed Roundtrip" $ do
        it "Index @'Hardened _" (property prop_roundtripIndexedHard)
        it "Index @'Soft _" (property prop_roundtripIndexedSoft)

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

prop_roundtripIndexedHard :: Index 'WholeDomain 'AccountK -> Property
prop_roundtripIndexedHard ix = (fromWord32 . toWord32) ix === Just ix

prop_roundtripIndexedSoft :: Index 'Soft 'PaymentK -> Property
prop_roundtripIndexedSoft ix = (fromWord32 . toWord32) ix === Just ix

{-------------------------------------------------------------------------------
                             Arbitrary Instances
-------------------------------------------------------------------------------}

instance Arbitrary ScrubbedBytes where
    arbitrary = fromString <$> arbitrary
