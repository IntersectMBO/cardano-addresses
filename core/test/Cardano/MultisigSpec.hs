{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.MultisigSpec
    ( spec
    ) where

import Prelude

import Cardano.Address.Derivation
    ( Depth (..), GenMasterKey (..), HardDerivation (..), XPrv, toXPub )
import Cardano.Address.Style.Shelley
    ( Shelley (..), deriveMultisigPrivateKey )
import Cardano.Mnemonic
    ( mkSomeMnemonic )
import Cardano.Multisig
    ( MultisigScript (..), fromVerificationKey, toCBOR, toScriptHash )
import Codec.Binary.Encoding
    ( AbstractEncoding (..), encode )
import Test.Hspec
    ( Spec, describe, it, shouldBe )

import qualified Data.Text.Encoding as T

spec :: Spec
spec = do
    describe "Multisig CBOR and hashes - golden tests" $ do

        let mnemonic = [ "test", "child", "burst", "immense", "armed", "parrot"
                       , "company", "walk", "dog" ]
        let (Right mw) = mkSomeMnemonic @'[9,12,15,18,21,24] mnemonic
        let sndFactor = mempty
        let rootK = genMasterKeyFromMnemonic mw sndFactor :: Shelley 'RootK XPrv
        let accXPrv = deriveAccountPrivateKey rootK minBound

        let index1 = minBound
        let multisigXPub1 = toXPub <$> deriveMultisigPrivateKey accXPrv index1
        -- "deeae4e895d8d57378125ed4fd540f9bf245d59f7936a504379cfc1e"
        let verKeyHash1 = RequireSignatureOf $ fromVerificationKey multisigXPub1

        let index2 = toEnum 0x00000001
        let multisigXPub2 = toXPub <$> deriveMultisigPrivateKey accXPrv index2
        -- "60a3bf69aa748f9934b64357d9f1ca202f1a768aaf57263aedca8d5f"
        let verKeyHash2 = RequireSignatureOf $ fromVerificationKey multisigXPub2

        let index3 = toEnum 0x00000002
        let multisigXPub3 = toXPub <$> deriveMultisigPrivateKey accXPrv index3
        -- "ffcbb72393215007d9a0aa02b7430080409cd8c053fd4f5b4d905053"
        let verKeyHash3 = RequireSignatureOf $ fromVerificationKey multisigXPub3

        let index4 = toEnum 0x00000003
        let multisigXPub4 = toXPub <$> deriveMultisigPrivateKey accXPrv index4
        -- "96834025cdca063ce9c32dfae6bc6a3e47f8da07ee4fb8e1a3901559"
        let verKeyHash4 = RequireSignatureOf $ fromVerificationKey multisigXPub4

        it "RequireSignatureOf index=0" $ do
            (toHexText (toCBOR verKeyHash1) ) `shouldBe`
                "82008200581cdeeae4e895d8d57378125ed4fd540f9bf245d59f7936a504379cfc1e"
            (toHexText (toScriptHash verKeyHash1) )  `shouldBe`
                "59fd497a34ac3e5abf2c8a703e3aaf3a2750e207b139d65d08d2c1b3"
        it "RequireSignatureOf index=1" $ do
            (toHexText (toCBOR verKeyHash2) ) `shouldBe`
                "82008200581c60a3bf69aa748f9934b64357d9f1ca202f1a768aaf57263aedca8d5f"
        it "RequireSignatureOf index=2" $ do
            (toHexText (toCBOR verKeyHash3) ) `shouldBe`
                "82008200581cffcbb72393215007d9a0aa02b7430080409cd8c053fd4f5b4d905053"
        it "RequireSignatureOf index=3" $ do
            (toHexText (toCBOR verKeyHash4) ) `shouldBe`
                "82008200581c96834025cdca063ce9c32dfae6bc6a3e47f8da07ee4fb8e1a3901559"

        it "RequireAllOf for index=0 and index=1 keys" $ do
            let script = RequireAllOf [verKeyHash1, verKeyHash2]
            (toHexText (toCBOR script) ) `shouldBe`
                "82008201828200581cdeeae4e895d8d57378125ed4fd540f9bf245d59f7936a50437\
                \9cfc1e8200581c60a3bf69aa748f9934b64357d9f1ca202f1a768aaf57263aedca8d5f"
        it "RequireAllOf for index=0, index=1 and index=2 keys" $ do
            let script = RequireAllOf [verKeyHash1, verKeyHash2, verKeyHash3]
            (toHexText (toCBOR script) ) `shouldBe`
                "82008201838200581cdeeae4e895d8d57378125ed4fd540f9bf245d59f7936a504379cfc1e8200\
                \581c60a3bf69aa748f9934b64357d9f1ca202f1a768aaf57263aedca8d5f8200581cffcbb72393\
                \215007d9a0aa02b7430080409cd8c053fd4f5b4d905053"

        it "RequireAnyOf for index=0 and index=1 keys" $ do
            let script = RequireAnyOf [verKeyHash1, verKeyHash2]
            (toHexText (toCBOR script) ) `shouldBe`
                "82008202828200581cdeeae4e895d8d57378125ed4fd540f9bf245d59f7936a50437\
                \9cfc1e8200581c60a3bf69aa748f9934b64357d9f1ca202f1a768aaf57263aedca8d5f"
        it "RequireAllOf for index=0, index=1 and index=2 keys" $ do
            let script = RequireAnyOf [verKeyHash1, verKeyHash2, verKeyHash3]
            (toHexText (toCBOR script) ) `shouldBe`
                "82008202838200581cdeeae4e895d8d57378125ed4fd540f9bf245d59f7936a504379cfc1e8200\
                \581c60a3bf69aa748f9934b64357d9f1ca202f1a768aaf57263aedca8d5f8200581cffcbb72393\
                \215007d9a0aa02b7430080409cd8c053fd4f5b4d905053"

        it "RequireMOf 1 out of index=0 and index=1 keys" $ do
            let script = RequireMOf 1 [verKeyHash1, verKeyHash2]
            (toHexText (toCBOR script) ) `shouldBe`
                "8200830301828200581cdeeae4e895d8d57378125ed4fd540f9bf245d59f7936a504\
                \379cfc1e8200581c60a3bf69aa748f9934b64357d9f1ca202f1a768aaf57263aedca8d5f"
        it "RequireAllOf 2 out of index=0, index=1, index=2 and index=3 keys" $ do
            let script = RequireMOf 2 [verKeyHash1, verKeyHash2, verKeyHash3, verKeyHash4]
            (toHexText (toCBOR script) ) `shouldBe`
                "8200830302848200581cdeeae4e895d8d57378125ed4fd540f9bf245d59f7936a504379cfc1e82\
                \00581c60a3bf69aa748f9934b64357d9f1ca202f1a768aaf57263aedca8d5f8200581cffcbb723\
                \93215007d9a0aa02b7430080409cd8c053fd4f5b4d9050538200581c96834025cdca063ce9c32d\
                \fae6bc6a3e47f8da07ee4fb8e1a3901559"

        it "nested 1" $ do
            let nested = RequireAllOf [verKeyHash3, verKeyHash4]
            let script = RequireMOf 2 [verKeyHash1, verKeyHash2, nested]
            (toHexText (toCBOR script) ) `shouldBe`
                "8200830302838200581cdeeae4e895d8d57378125ed4fd540f9bf245d59f7936a504379cfc1e8\
                \200581c60a3bf69aa748f9934b64357d9f1ca202f1a768aaf57263aedca8d5f8201828200581c\
                \ffcbb72393215007d9a0aa02b7430080409cd8c053fd4f5b4d9050538200581c96834025cdca0\
                \63ce9c32dfae6bc6a3e47f8da07ee4fb8e1a3901559"

        it "nested 2" $ do
            let nested = RequireAnyOf [verKeyHash2, verKeyHash3, verKeyHash4]
            let script = RequireAllOf [verKeyHash1, nested]
            (toHexText (toCBOR script) ) `shouldBe`
                "82008201828200581cdeeae4e895d8d57378125ed4fd540f9bf245d59f7936a504379cfc1e82\
                \02838200581c60a3bf69aa748f9934b64357d9f1ca202f1a768aaf57263aedca8d5f8200581c\
                \ffcbb72393215007d9a0aa02b7430080409cd8c053fd4f5b4d9050538200581c96834025cdca\
                \063ce9c32dfae6bc6a3e47f8da07ee4fb8e1a3901559"

        it "nested 3" $ do
            let nested' = RequireAnyOf [verKeyHash3, verKeyHash4]
            let nested = RequireAllOf [verKeyHash1, nested']
            let script = RequireMOf 1 [verKeyHash1, nested]
            (toHexText (toCBOR script) ) `shouldBe`
                "8200830301828200581cdeeae4e895d8d57378125ed4fd540f9bf245d59f7936a504379cfc1e8\
                \201828200581cdeeae4e895d8d57378125ed4fd540f9bf245d59f7936a504379cfc1e82028282\
                \00581cffcbb72393215007d9a0aa02b7430080409cd8c053fd4f5b4d9050538200581c9683402\
                \5cdca063ce9c32dfae6bc6a3e47f8da07ee4fb8e1a3901559"

  where
    toHexText = T.decodeUtf8 . encode EBase16
