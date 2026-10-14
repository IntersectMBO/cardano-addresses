{-# LANGUAGE FlexibleContexts #-}

module Command.Address.RewardSpec
    ( spec
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPub, xpubToBytes )
import Codec.Binary.Encoding
    ( AbstractEncoding (..), Encoding, encode )
import Data.ByteString
    ( ByteString )
import Test.Hspec
    ( Spec, SpecWith, it, shouldBe, shouldContain )
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

import qualified Cardano.Codec.Bech32.Prefixes as CIP5
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

spec :: Spec
spec = describeCmd [ "address", "stake" ] $ do
    specShelley defaultPhrase "1852H/1815H/0H/2/0" 0
        "stake_test1ura3dk68y6echdmfmnvm8mej8u5truwv8ufmv830w5a45tcsfhtt2"

    specShelley defaultPhrase "1852H/1815H/0H/2/0" 3
        "stake1u0a3dk68y6echdmfmnvm8mej8u5truwv8ufmv830w5a45tchw5z0e"

    specShelleyFromKeyHash defaultPhrase "1852H/1815H/0H/2/0" 0
        "stake_test1ura3dk68y6echdmfmnvm8mej8u5truwv8ufmv830w5a45tcsfhtt2"

    specShelleyFromKeyHash defaultPhrase "1852H/1815H/0H/2/0" 3
        "stake1u0a3dk68y6echdmfmnvm8mej8u5truwv8ufmv830w5a45tchw5z0e"

    specMalformedNetwork "💩"

    specInvalidNetwork "42"

    prop "an extended key and its base16/base58 forms give the same stake address"
        (withMaxSuccess 20 propShelleyFromXPubRaw)

    it "rejects a bare 28-byte credential as ambiguous (key or script hash)" $ do
        (out, err) <- cli [ "address", "stake", "--network-tag", "testnet" ]
            "000102030405060708090a0b0c0d0e0f101112131415161718191a1b"
        out `shouldBe` ""
        err `shouldContain` "ambiguous"

specShelley :: [String] -> String -> Int -> String -> SpecWith ()
specShelley phrase path networkTag want = it ("golden shelley (payment) " <> path) $ do
    out <- cli [ "key", "from-recovery-phrase", "shelley" ] (unwords phrase)
       >>= cli [ "key", "child", path ]
       >>= cli [ "key", "public", "--with-chain-code" ]
       >>= cli [ "address", "stake", "--network-tag", show networkTag ]
    out `shouldBe` want

specShelleyFromKeyHash :: [String] -> String -> Int -> String -> SpecWith ()
specShelleyFromKeyHash phrase path networkTag want = it ("golden shelley (payment) " <> path) $ do
    out <- cli [ "key", "from-recovery-phrase", "shelley" ] (unwords phrase)
       >>= cli [ "key", "child", path ]
       >>= cli [ "key", "public", "--with-chain-code" ]
       >>= cli [ "key", "hash" ]
       >>= cli [ "address", "stake", "--network-tag", show networkTag ]
    out `shouldBe` want

specMalformedNetwork :: String -> SpecWith ()
specMalformedNetwork networkTag = it ("malformed network " <> networkTag) $ do
    (out, err) <- cli [ "key", "from-recovery-phrase", "shelley" ] (unwords defaultPhrase)
        >>= cli [ "key", "public", "--with-chain-code" ]
        >>= cli [ "address", "stake", "--network-tag", networkTag ]
    out `shouldBe` ""
    err `shouldContain` "Invalid network tag"
    err `shouldContain` "Usage"

specInvalidNetwork :: String -> SpecWith ()
specInvalidNetwork networkTag = it ("invalid network " <> networkTag) $ do
    (out, err) <- cli [ "key", "from-recovery-phrase", "shelley" ] (unwords defaultPhrase)
        >>= cli [ "key", "public", "--with-chain-code" ]
        >>= cli [ "address", "stake", "--network-tag", networkTag ]
    out `shouldBe` ""
    err `shouldContain` "Invalid network tag"

defaultPhrase :: [String]
defaultPhrase =
    [ "pole", "pulse", "wolf", "blame", "chronic"
    , "ship", "vivid", "tree", "small", "onion"
    , "host", "accident", "burden", "lazy", "swarm"
    ]

-- | An extended stake key can be supplied as bech32 (@stake_xvk@) or as raw
-- base16/base58 bytes; all three must yield the same stake address.
propShelleyFromXPubRaw :: XPub -> Property
propShelleyFromXPubRaw xpub = monadicIO $ do
    let bytes = xpubToBytes xpub
        asBech32 = T.unpack $ T.decodeUtf8 $ encode (EBech32 CIP5.stake_xvk) bytes
        asBase16 = encodeBytes EBase16 bytes
        asBase58 = encodeBytes EBase58 bytes
    fromBech32 <- run (cli [ "address", "stake", "--network-tag", "testnet" ] asBech32 :: IO String)
    fromBase16 <- run (cli [ "address", "stake", "--network-tag", "testnet" ] asBase16 :: IO String)
    fromBase58 <- run (cli [ "address", "stake", "--network-tag", "testnet" ] asBase58 :: IO String)
    assert (fromBech32 == fromBase16 && fromBech32 == fromBase58)

encodeBytes :: Encoding -> ByteString -> String
encodeBytes enc = T.unpack . T.decodeUtf8 . encode enc
