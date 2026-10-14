{-# LANGUAGE FlexibleContexts #-}

module Command.Address.PaymentSpec
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
spec = describeCmd [ "address", "payment" ] $ do
    specShelleyFromXPub defaultPhrase "1852H/1815H/0H/0/0" "0"
        "addr_test1vpu5vlrf4xkxv2qpwngf6cjhtw542ayty80v8dyr49rf5eg57c2qv"

    specShelleyFromXPub defaultPhrase "1852H/1815H/0H/0/0" "3"
        "addr1vdu5vlrf4xkxv2qpwngf6cjhtw542ayty80v8dyr49rf5eg0m9a08"

    specShelleyFromXPub defaultPhrase "1852H/1815H/0H/0/0" "testnet"
        "addr_test1vpu5vlrf4xkxv2qpwngf6cjhtw542ayty80v8dyr49rf5eg57c2qv"

    specShelleyFromXPub defaultPhrase "1852H/1815H/0H/0/0" "mainnet"
        "addr1v9u5vlrf4xkxv2qpwngf6cjhtw542ayty80v8dyr49rf5eg0kvk0f"

    specShelleyFromPub defaultPhrase "1852H/1815H/0H/0/0" "0"
        "addr_test1vpu5vlrf4xkxv2qpwngf6cjhtw542ayty80v8dyr49rf5eg57c2qv"

    specShelleyFromPub defaultPhrase "1852H/1815H/0H/0/0" "3"
        "addr1vdu5vlrf4xkxv2qpwngf6cjhtw542ayty80v8dyr49rf5eg0m9a08"

    specShelleyFromPub defaultPhrase "1852H/1815H/0H/0/0" "testnet"
        "addr_test1vpu5vlrf4xkxv2qpwngf6cjhtw542ayty80v8dyr49rf5eg57c2qv"

    specShelleyFromPub defaultPhrase "1852H/1815H/0H/0/0" "mainnet"
        "addr1v9u5vlrf4xkxv2qpwngf6cjhtw542ayty80v8dyr49rf5eg0kvk0f"

    specShelleyFromKeyHash defaultPhrase "1852H/1815H/0H/0/0" "0"
        "addr_test1vpu5vlrf4xkxv2qpwngf6cjhtw542ayty80v8dyr49rf5eg57c2qv"

    specShelleyFromKeyHash defaultPhrase "1852H/1815H/0H/0/0" "3"
        "addr1vdu5vlrf4xkxv2qpwngf6cjhtw542ayty80v8dyr49rf5eg0m9a08"

    specShelleyFromKeyHash defaultPhrase "1852H/1815H/0H/0/0" "testnet"
        "addr_test1vpu5vlrf4xkxv2qpwngf6cjhtw542ayty80v8dyr49rf5eg57c2qv"

    specShelleyFromKeyHash defaultPhrase "1852H/1815H/0H/0/0" "mainnet"
        "addr1v9u5vlrf4xkxv2qpwngf6cjhtw542ayty80v8dyr49rf5eg0kvk0f"

    specMalformedNetwork "💩"

    specInvalidNetwork "42"
    specInvalidNetwork "staging"

    prop "an extended key and its base16/base58 forms give the same payment address"
        (withMaxSuccess 20 propShelleyFromXPubRaw)

    it "rejects a bare 28-byte credential as ambiguous (key or script hash)" $ do
        (out, err) <- cli [ "address", "payment", "--network-tag", "testnet" ]
            "000102030405060708090a0b0c0d0e0f101112131415161718191a1b"
        out `shouldBe` ""
        err `shouldContain` "ambiguous"

specShelleyFromXPub :: [String] -> String -> String -> String -> SpecWith ()
specShelleyFromXPub phrase path networkTag want = it ("golden shelley (payment) " <> path) $ do
    out <- cli [ "key", "from-recovery-phrase", "shelley" ] (unwords phrase)
       >>= cli [ "key", "child", path ]
       >>= cli [ "key", "public", "--with-chain-code" ]
       >>= cli [ "address", "payment", "--network-tag", networkTag ]
    out `shouldBe` want

specShelleyFromPub :: [String] -> String -> String -> String -> SpecWith ()
specShelleyFromPub phrase path networkTag want = it ("golden shelley (payment) " <> path) $ do
    out <- cli [ "key", "from-recovery-phrase", "shelley" ] (unwords phrase)
       >>= cli [ "key", "child", path ]
       >>= cli [ "key", "public", "--without-chain-code" ]
       >>= cli [ "address", "payment", "--network-tag", networkTag ]
    out `shouldBe` want

specShelleyFromKeyHash :: [String] -> String -> String -> String -> SpecWith ()
specShelleyFromKeyHash phrase path networkTag want = it ("golden shelley (payment) " <> path) $ do
    out <- cli [ "key", "from-recovery-phrase", "shelley" ] (unwords phrase)
       >>= cli [ "key", "child", path ]
       >>= cli [ "key", "public", "--with-chain-code" ]
       >>= cli [ "key", "hash" ]
       >>= cli [ "address", "payment", "--network-tag", networkTag ]
    out `shouldBe` want

specMalformedNetwork :: String -> SpecWith ()
specMalformedNetwork networkTag = it ("malformed network " <> networkTag) $ do
    (out, err) <- cli [ "key", "from-recovery-phrase", "shelley" ] (unwords defaultPhrase)
        >>= cli [ "key", "public", "--with-chain-code" ]
        >>= cli [ "address", "payment", "--network-tag", networkTag ]
    out `shouldBe` ""
    err `shouldContain` "Invalid network tag. Must be an integer value or one of the allowed keywords:"
    err `shouldContain` "Usage"

specInvalidNetwork :: String -> SpecWith ()
specInvalidNetwork networkTag = it ("invalid network " <> networkTag) $ do
    (out, err) <- cli [ "key", "from-recovery-phrase", "shelley" ] (unwords defaultPhrase)
        >>= cli [ "key", "public", "--with-chain-code" ]
        >>= cli [ "address", "payment", "--network-tag", networkTag ]
    out `shouldBe` ""
    err `shouldContain` "Invalid network tag."

defaultPhrase :: [String]
defaultPhrase =
    [ "art", "forum", "devote", "street", "sure"
    , "rather", "head", "chuckle", "guard", "poverty"
    , "release", "quote", "oak", "craft", "enemy"
    ]

-- | An extended payment key can be supplied as bech32 (@addr_xvk@) or as raw
-- base16/base58 bytes; all three must yield the same payment address.
propShelleyFromXPubRaw :: XPub -> Property
propShelleyFromXPubRaw xpub = monadicIO $ do
    let bytes = xpubToBytes xpub
        asBech32 = T.unpack $ T.decodeUtf8 $ encode (EBech32 CIP5.addr_xvk) bytes
        asBase16 = encodeBytes EBase16 bytes
        asBase58 = encodeBytes EBase58 bytes
    fromBech32 <- run (cli [ "address", "payment", "--network-tag", "testnet" ] asBech32 :: IO String)
    fromBase16 <- run (cli [ "address", "payment", "--network-tag", "testnet" ] asBase16 :: IO String)
    fromBase58 <- run (cli [ "address", "payment", "--network-tag", "testnet" ] asBase58 :: IO String)
    assert (fromBech32 == fromBase16 && fromBech32 == fromBase58)

encodeBytes :: Encoding -> ByteString -> String
encodeBytes enc = T.unpack . T.decodeUtf8 . encode enc
