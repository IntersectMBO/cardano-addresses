{-# LANGUAGE FlexibleContexts #-}

module Command.Key.HashSpec
    ( spec
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPub, hashCredential, xpubToBytes )
import Codec.Binary.Encoding
    ( AbstractEncoding (..), Encoding, encode )
import Data.ByteString
    ( ByteString )
import Test.Hspec
    ( Spec, SpecWith, it, shouldBe, shouldContain, shouldStartWith )
import Test.Hspec.QuickCheck
    ( prop )
import Test.QuickCheck
    ( Property, withMaxSuccess )
import Test.QuickCheck.Monadic
    ( assert, monadicIO, run )
import Test.Utils
    ( cli, describeCmd )

import Test.Arbitrary
    ()

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

spec :: Spec
spec = describeCmd [ "key", "hash" ] $ do
    specKeyNotPublic
    specKeyPublic "icarus" "addr_vkh"   "1852H/1815H/0H/0/0" "--without-chain-code"
    specKeyPublic "icarus" "addr_vkh"   "1852H/1815H/0H/1/0" "--without-chain-code"
    specKeyPublic "icarus" "stake_vkh"  "1852H/1815H/0H/2/0" "--without-chain-code"
    specKeyPublic "shared" "addr_shared_vkh"   "1854H/1815H/0H/0/0" "--without-chain-code"
    specKeyPublic "shared" "stake_shared_vkh"  "1854H/1815H/0H/2/0" "--without-chain-code"

    specKeyPublic "icarus" "addr_vkh"   "1852H/1815H/0H/0/0" "--with-chain-code"
    specKeyPublic "icarus" "addr_vkh"   "1852H/1815H/0H/1/0" "--with-chain-code"
    specKeyPublic "icarus" "stake_vkh"  "1852H/1815H/0H/2/0" "--with-chain-code"
    specKeyPublic "shared" "addr_shared_vkh"   "1854H/1815H/0H/0/0" "--with-chain-code"
    specKeyPublic "shared" "stake_shared_vkh"  "1854H/1815H/0H/2/0" "--with-chain-code"

    specKeyPublic "shelley" "policy_vkh"   "1855H/1815H/0H" "--with-chain-code"
    specKeyPublic "shelley" "policy_vkh"   "1855H/1815H/0H" "--without-chain-code"

    specKeyPublic "shelley" "drep_vkh"   "1852H/1815H/0H/3/0" "--with-chain-code"
    specKeyPublic "shelley" "drep_vkh"   "1852H/1815H/0H/3/0" "--without-chain-code"
    specKeyPublic "shelley" "cc_cold_vkh"   "1852H/1815H/0H/4/0" "--with-chain-code"
    specKeyPublic "shelley" "cc_cold_vkh"   "1852H/1815H/0H/4/0" "--without-chain-code"
    specKeyPublic "shelley" "cc_hot_vkh"   "1852H/1815H/0H/5/0" "--with-chain-code"
    specKeyPublic "shelley" "cc_hot_vkh"   "1852H/1815H/0H/5/0" "--without-chain-code"

    specKeyPublicCred "shelley" "drep1"   "1852H/1815H/0H/3/0" "--with-chain-code"
    specKeyPublicCred "shelley" "drep1"   "1852H/1815H/0H/3/0" "--without-chain-code"
    specKeyPublicCred "shelley" "cc_cold1"   "1852H/1815H/0H/4/0" "--with-chain-code"
    specKeyPublicCred "shelley" "cc_cold1"   "1852H/1815H/0H/4/0" "--without-chain-code"
    specKeyPublicCred "shelley" "cc_hot1"   "1852H/1815H/0H/5/0" "--with-chain-code"
    specKeyPublicCred "shelley" "cc_hot1"   "1852H/1815H/0H/5/0" "--without-chain-code"

    prop "a raw public key yields its hex-encoded hash, in any raw encoding"
        (withMaxSuccess 20 propKeyHashRaw)

propKeyHashRaw :: XPub -> Property
propKeyHashRaw xpub = monadicIO $ do
    let bytes = xpubToBytes xpub
        asBase16 = encodeBytes EBase16 bytes
        asBase58 = encodeBytes EBase58 bytes
        expected = T.unpack $ T.decodeUtf8 $ encode EBase16 $ hashCredential $ BS.take 32 bytes
    fromBase16 <- run (cli [ "key", "hash" ] asBase16 :: IO String)
    fromBase58 <- run (cli [ "key", "hash" ] asBase58 :: IO String)
    assert (fromBase16 == expected && fromBase58 == expected)

encodeBytes :: Encoding -> ByteString -> String
encodeBytes enc = T.unpack . T.decodeUtf8 . encode enc

specKeyNotPublic :: SpecWith ()
specKeyNotPublic = it "fail if key isn't public" $ do
    (out, err) <- cli [ "recovery-phrase", "generate" ] ""
              >>= cli [ "key", "from-recovery-phrase", "icarus" ]
              >>= cli [ "key", "hash" ]

    out `shouldBe` ""
    err `shouldContain` "Invalid human-readable prefix."

specKeyPublic :: String -> String -> String -> String -> SpecWith ()
specKeyPublic style hrp path cc = it "succeeds if key is public" $ do
    out <- cli [ "recovery-phrase", "generate" ] ""
       >>= cli [ "key", "from-recovery-phrase", style ]
       >>= cli [ "key", "child", path ]
       >>= cli [ "key", "public", cc ]
       >>= cli [ "key", "hash", "--cip-0105" ]
    out `shouldStartWith` hrp

specKeyPublicCred :: String -> String -> String -> String -> SpecWith ()
specKeyPublicCred style hrp path cc = it "succeeds if key is public" $ do
    out <- cli [ "recovery-phrase", "generate" ] ""
       >>= cli [ "key", "from-recovery-phrase", style ]
       >>= cli [ "key", "child", path ]
       >>= cli [ "key", "public", cc ]
       >>= cli [ "key", "hash" ]
    out `shouldStartWith` hrp
