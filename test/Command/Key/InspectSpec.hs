{-# LANGUAGE FlexibleContexts #-}

module Command.Key.InspectSpec
    ( spec
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPub
    , hashCredential
    , xprvChainCode
    , xprvPrivateKey
    , xprvToBytes
    , xpubChainCode
    , xpubPublicKey
    , xpubToBytes
    )
import Cardano.Address.KeyHash
    ( GovernanceType (..), KeyHash (..), KeyRole (..), keyHashToText )
import Codec.Binary.Bech32
    ( HumanReadablePart )
import Codec.Binary.Encoding
    ( AbstractEncoding (..), Encoding, encode )
import Data.ByteString
    ( ByteString )
import Test.Hspec
    ( Spec, SpecWith, it, shouldBe, shouldContain )
import Test.Hspec.QuickCheck
    ( prop )
import Test.QuickCheck
    ( Property, arbitrary, generate, withMaxSuccess )
import Test.QuickCheck.Monadic
    ( assert, monadicIO, run )
import Test.Utils
    ( cli, describeCmd )

import Test.Arbitrary
    ()

import qualified Cardano.Codec.Bech32.Prefixes as CIP5
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

spec :: Spec
spec = describeCmd [ "key", "inspect" ] $ do
    specInspectPrivate CIP5.root_xsk
    specInspectPrivate CIP5.acct_xsk
    specInspectPrivate CIP5.addr_xsk
    specInspectPrivate CIP5.stake_xsk
    specInspectPrivate CIP5.root_shared_xsk
    specInspectPrivate CIP5.acct_shared_xsk
    specInspectPrivate CIP5.addr_shared_xsk
    specInspectPrivate CIP5.stake_shared_xsk
    specInspectPrivate CIP5.policy_xsk
    specInspectPrivate CIP5.drep_xsk
    specInspectPrivate CIP5.cc_cold_xsk
    specInspectPrivate CIP5.cc_hot_xsk

    specInspectPrivateNonExtended CIP5.addr_sk
    specInspectPrivateNonExtended CIP5.stake_sk
    specInspectPrivateNonExtended CIP5.policy_sk
    specInspectPrivateNonExtended CIP5.drep_sk
    specInspectPrivateNonExtended CIP5.cc_cold_sk
    specInspectPrivateNonExtended CIP5.cc_hot_sk

    specInspectPublic CIP5.root_xvk
    specInspectPublic CIP5.acct_xvk
    specInspectPublic CIP5.addr_xvk
    specInspectPublic CIP5.stake_xvk
    specInspectPublic CIP5.policy_xvk
    specInspectPublic CIP5.root_shared_xvk
    specInspectPublic CIP5.acct_shared_xvk
    specInspectPublic CIP5.addr_shared_xvk
    specInspectPublic CIP5.stake_shared_xvk
    specInspectPublic CIP5.drep_xvk
    specInspectPublic CIP5.cc_cold_xvk
    specInspectPublic CIP5.cc_hot_xvk

    specInspectPublicNonExtended CIP5.addr_vk
    specInspectPublicNonExtended CIP5.stake_vk
    specInspectPublicNonExtended CIP5.policy_vk
    specInspectPublicNonExtended CIP5.drep_vk
    specInspectPublicNonExtended CIP5.cc_cold_vk
    specInspectPublicNonExtended CIP5.cc_hot_vk

    specInspectCredential Payment CIP0129
    specInspectCredential Delegation CIP0129
    specInspectCredential PaymentShared CIP0129
    specInspectCredential DelegationShared CIP0129
    specInspectCredential Policy CIP0129
    specInspectCredential Representative CIP0129
    specInspectCredential Representative CIP0105
    specInspectCredential CommitteeCold CIP0129
    specInspectCredential CommitteeCold CIP0105
    specInspectCredential CommitteeHot CIP0129
    specInspectCredential CommitteeHot CIP0105

    specInspectHex

    specInspectErrors

    prop "base16, base58 and bech32 extended public keys inspect identically"
        (withMaxSuccess 50 . propInspectXPubEncoding)
    prop "base16, base58 and bech32 non-extended public keys inspect identically"
        (withMaxSuccess 50 . propInspectNonExtendedEncoding)
    prop "a CIP-0129 governance credential and its hex form inspect identically"
        (withMaxSuccess 50 . propInspectCredentialEncoding)

-- | Whatever encoding is used to feed the same extended public key, the
-- inspected output must be identical.
propInspectXPubEncoding :: XPub -> Property
propInspectXPubEncoding xpub = monadicIO $ do
    let bytes = xpubToBytes xpub
    fromBech32 <- run $ inspectKey $ encodeBytes (EBech32 CIP5.addr_xvk) bytes
    fromBase16 <- run $ inspectKey $ encodeBytes EBase16 bytes
    fromBase58 <- run $ inspectKey $ encodeBytes EBase58 bytes
    assert (fromBech32 == fromBase16)
    assert (fromBase16 == fromBase58)

-- | Same as 'propInspectXPubEncoding' but for non-extended public keys (raw
-- 32-byte payloads, no chain code).
propInspectNonExtendedEncoding :: XPub -> Property
propInspectNonExtendedEncoding xpub = monadicIO $ do
    let bytes = xpubPublicKey xpub
    fromBech32 <- run $ inspectKey $ encodeBytes (EBech32 CIP5.addr_vk) bytes
    fromBase16 <- run $ inspectKey $ encodeBytes EBase16 bytes
    fromBase58 <- run $ inspectKey $ encodeBytes EBase58 bytes
    assert (fromBech32 == fromBase16)
    assert (fromBase16 == fromBase58)

-- | A governance credential can be given either as bech32 (CIP-0129) or as the
-- 29-byte hex form (header byte + digest). Both must yield the same output.
propInspectCredentialEncoding :: XPub -> Property
propInspectCredentialEncoding xpub = monadicIO $ do
    let digest = hashCredential (xpubPublicKey xpub)
        asBech32 = T.unpack $ keyHashToText (KeyHash Representative digest) CIP0129
        asBase16 = encodeBytes EBase16 (BS.cons 0x22 digest)
    fromBech32 <- run $ inspectKey asBech32
    fromBase16 <- run $ inspectKey asBase16
    assert (fromBech32 == fromBase16)

specInspectHex :: SpecWith ()
specInspectHex = do
    it "can inspect a hex-encoded extended public key" $ do
        xpub <- generate arbitrary
        let pub = base16 $ xpubPublicKey xpub
        let cc  = base16 $ xpubChainCode xpub
        out <- cli [ "key", "inspect" ] (base16 $ xpubToBytes xpub)
        out `shouldContain` "public"
        out `shouldContain` pub
        out `shouldContain` cc

    it "can inspect a hex-encoded extended private key" $ do
        xprv <- generate arbitrary
        let prv = base16 $ xprvPrivateKey xprv
        let cc  = base16 $ xprvChainCode xprv
        out <- cli [ "key", "inspect" ] (base16 $ xprvToBytes xprv)
        out `shouldContain` "private"
        out `shouldContain` prv
        out `shouldContain` cc

    it "can inspect a hex-encoded non-extended public key" $ do
        xpub <- generate arbitrary
        let pub = base16 $ xpubPublicKey xpub
        out <- cli [ "key", "inspect" ] (base16 $ xpubPublicKey xpub)
        out `shouldContain` "public"
        out `shouldContain` pub

    it "can inspect a hex-encoded key hash" $ do
        payload <- credentialDigest
        let hash = base16 payload
        out <- cli [ "key", "inspect" ] hash
        out `shouldContain` "hash"
        out `shouldContain` "unknown"
        out `shouldContain` hash

    it "can inspect a hex-encoded CIP-0129 credential" $ do
        payload <- credentialDigest
        let credential = base16 $ BS.cons 0x22 payload
        out <- cli [ "key", "inspect" ] credential
        out `shouldContain` "hash"
        out `shouldContain` "representative"
        out `shouldContain` base16 payload

specInspectCredential :: KeyRole -> GovernanceType -> SpecWith ()
specInspectCredential role' govType = it "can inspect a key hash" $ do
    payload <- credentialDigest
    let encoded = T.unpack $ keyHashToText (KeyHash role' payload) govType
    out <- cli [ "key", "inspect" ] encoded
    out `shouldContain` "hash"
    out `shouldContain` base16 payload

credentialDigest :: IO ByteString
credentialDigest = do
    xpub <- generate arbitrary
    pure $ hashCredential (xpubPublicKey xpub)

specInspectErrors :: SpecWith ()
specInspectErrors =
    it "surfaces a bech32 decoding error" $ do
        let malformed =
                "addr_xvk1qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq"
        (out, err) <- cli [ "key", "inspect" ] malformed
        out `shouldBe` ""
        err `shouldContain` "Bech32 error"

specInspectPrivate :: HumanReadablePart -> SpecWith ()
specInspectPrivate hrp = it "can inspect private key" $ do
    xprv <- generate arbitrary
    let cc  = base16 $ xprvChainCode xprv
    let prv = base16 $ xprvPrivateKey xprv
    out <- cli [ "key", "inspect" ] (bech32 hrp $ xprvToBytes xprv)
    out `shouldContain` "private"
    out `shouldContain` cc
    out `shouldContain` prv

specInspectPublic :: HumanReadablePart -> SpecWith ()
specInspectPublic hrp = it "can inspect public key" $ do
    xpub <- generate arbitrary
    let cc  = base16 $ xpubChainCode xpub
    let pub = base16 $ xpubPublicKey xpub
    out <- cli [ "key", "inspect" ] (bech32 hrp $ xpubToBytes xpub)
    out `shouldContain` "public"
    out `shouldContain` cc
    out `shouldContain` pub

specInspectPrivateNonExtended :: HumanReadablePart -> SpecWith ()
specInspectPrivateNonExtended hrp = it "can inspect non-extended private key" $ do
    xprv <- generate arbitrary
    let prv = base16 $ xprvPrivateKey xprv
    out <- cli [ "key", "inspect" ] (bech32 hrp $ xprvPrivateKey xprv)
    out `shouldContain` "private"
    out `shouldContain` prv

specInspectPublicNonExtended :: HumanReadablePart -> SpecWith ()
specInspectPublicNonExtended hrp = it "can inspect non-extended public key" $ do
    xpub <- generate arbitrary
    let pub = base16 $ xpubPublicKey xpub
    out <- cli [ "key", "inspect" ] (bech32 hrp $ xpubPublicKey xpub)
    out `shouldContain` "public"
    out `shouldContain` pub

base16 :: ByteString -> String
base16 = T.unpack . T.decodeUtf8 . encode EBase16

encodeBytes :: Encoding -> ByteString -> String
encodeBytes enc = T.unpack . T.decodeUtf8 . encode enc

inspectKey :: String -> IO String
inspectKey = cli [ "key", "inspect" ]

bech32 :: HumanReadablePart -> ByteString -> String
bech32 hrp = T.unpack . T.decodeUtf8 . encode (EBech32 hrp)
