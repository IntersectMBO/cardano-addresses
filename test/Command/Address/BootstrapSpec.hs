{-# LANGUAGE FlexibleContexts #-}

module Command.Address.BootstrapSpec
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
spec = describeCmd [ "address", "bootstrap" ] $ do
    specIcarus defaultPhrase "44H/1815H/0H/0/0" "764824073"
        "Ae2tdPwUPEYz6ExfbWubiXPB6daUuhJxikMEb4eXRp5oKZBKZwrbJ2k7EZe"
    specIcarus defaultPhrase "44H/1815H/0H/0/1" "764824073"
        "Ae2tdPwUPEZCJUCuVgnysar8ZJeyKuhjXU35VNgKMMTcXWmS9zzYycmwKa4"
    specIcarus defaultPhrase "44H/1815H/0H/0/2" "764824073"
        "Ae2tdPwUPEZFJtMH1m5HvsaQZrmgLcVcyuk5TxYtdRHZFo8yV7yEnnJyqTs"

    specIcarus defaultPhrase "44H/1815H/0H/0/0" "mainnet"
        "Ae2tdPwUPEYz6ExfbWubiXPB6daUuhJxikMEb4eXRp5oKZBKZwrbJ2k7EZe"
    specIcarus defaultPhrase "44H/1815H/0H/0/1" "mainnet"
        "Ae2tdPwUPEZCJUCuVgnysar8ZJeyKuhjXU35VNgKMMTcXWmS9zzYycmwKa4"
    specIcarus defaultPhrase "44H/1815H/0H/0/2" "mainnet"
        "Ae2tdPwUPEZFJtMH1m5HvsaQZrmgLcVcyuk5TxYtdRHZFo8yV7yEnnJyqTs"

    specByron defaultPhrase "0H/0H" "764824073"
        "DdzFFzCqrhsf6hiTYkK5gBAhVDwg3SiaHiEL9wZLYU3WqLUpx6DP\
        \5ZRJr4rtNRXbVNfk89FCHCDR365647os9AEJ8MKZNvG7UKTpythG"
    specByron defaultPhrase "0H/1H" "764824073"
        "DdzFFzCqrhssdAorKQ7hGGPgNS3akoiuNG6YY7bb11hYrm1x716e\
        \akbz3yUppMS6X4t8WgM3Nx9CwjqZk3oNgm8s9yooJTs5AS7ptFT6"
    specByron defaultPhrase "0H/2H" "764824073"
        "DdzFFzCqrht4YH5irxboFprowLYgJLddd2iCQt5mrVyQS5CZFMeZ\
        \bQtzJsHf4a6YQhPPNxtqBVP3Drsy1tdjecqq1FK1m2oAR5J8EcVe"

    specByron defaultPhrase "0H/0H" "mainnet"
        "DdzFFzCqrhsf6hiTYkK5gBAhVDwg3SiaHiEL9wZLYU3WqLUpx6DP\
        \5ZRJr4rtNRXbVNfk89FCHCDR365647os9AEJ8MKZNvG7UKTpythG"
    specByron defaultPhrase "0H/1H" "mainnet"
        "DdzFFzCqrhssdAorKQ7hGGPgNS3akoiuNG6YY7bb11hYrm1x716e\
        \akbz3yUppMS6X4t8WgM3Nx9CwjqZk3oNgm8s9yooJTs5AS7ptFT6"
    specByron defaultPhrase "0H/2H" "mainnet"
        "DdzFFzCqrht4YH5irxboFprowLYgJLddd2iCQt5mrVyQS5CZFMeZ\
        \bQtzJsHf4a6YQhPPNxtqBVP3Drsy1tdjecqq1FK1m2oAR5J8EcVe"

    specInvalidNetwork "💩"

    prop "an extended key and its base16/base58 forms give the same bootstrap address"
        (withMaxSuccess 20 propBootstrapRaw)

propBootstrapRaw :: XPub -> Property
propBootstrapRaw xpub = monadicIO $ do
    let bytes = xpubToBytes xpub
        asBech32 = T.unpack $ T.decodeUtf8 $ encode (EBech32 CIP5.addr_xvk) bytes
        asBase16 = encodeBytes EBase16 bytes
        asBase58 = encodeBytes EBase58 bytes
    fromBech32 <- run (cli [ "address", "bootstrap", "--network-tag", "preview" ] asBech32 :: IO String)
    fromBase16 <- run (cli [ "address", "bootstrap", "--network-tag", "preview" ] asBase16 :: IO String)
    fromBase58 <- run (cli [ "address", "bootstrap", "--network-tag", "preview" ] asBase58 :: IO String)
    assert (fromBech32 == fromBase16 && fromBech32 == fromBase58)

encodeBytes :: Encoding -> ByteString -> String
encodeBytes enc = T.unpack . T.decodeUtf8 . encode enc

specIcarus :: [String] -> String -> String -> String -> SpecWith ()
specIcarus phrase path networkTag want = it ("golden icarus " <> path <> " (" <> networkTag <> ")") $ do
    out <- cli [ "key", "from-recovery-phrase", "icarus" ] (unwords phrase)
       >>= cli [ "key", "child", path ]
       >>= cli [ "key", "public", "--with-chain-code" ]
       >>= cli [ "address", "bootstrap", "--network-tag", networkTag ]
    out `shouldBe` want

specByron :: [String] -> String -> String -> String -> SpecWith ()
specByron phrase path networkTag want = it ("golden byron " <> path <> " (" <> networkTag <> ")") $ do
    rootPrv <- cli [ "key", "from-recovery-phrase", "byron" ] (unwords phrase)
    rootPub <- cli [ "key", "public", "--with-chain-code" ] rootPrv
    out <- cli [ "key", "child", path ] rootPrv
       >>= cli [ "key", "public", "--with-chain-code" ]
       >>= cli [ "address", "bootstrap", "--root", rootPub, "--network-tag", networkTag, path ]
    out `shouldBe` want

specInvalidNetwork :: String -> SpecWith ()
specInvalidNetwork networkTag = it ("invalid network " <> networkTag) $ do
    (out, err) <- cli [ "address", "bootstrap", "--network-tag", networkTag ] ""
    out `shouldBe` ""
    err `shouldContain` "Invalid network tag. Must be an integer value or one of the allowed keywords:"
    err `shouldContain` "Usage"

defaultPhrase :: [String]
defaultPhrase =
    [ "ghost", "buddy", "neutral", "broccoli", "face", "rack"
    , "relief", "odor", "swallow", "real", "once", "ecology"
    ]
