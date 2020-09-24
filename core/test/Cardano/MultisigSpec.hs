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
    ( MultisigScript (..)
    , VerificationKeyHash (..)
    , fromVerificationKey
    , toCBOR
    , toScriptHash
    )
import Codec.Binary.Encoding
    ( AbstractEncoding (..), encode )
import Test.Hspec
    ( Spec, describe, it, shouldBe )

import qualified Data.Text.Encoding as T

import qualified Debug.Trace as TR

spec :: Spec
spec = do
    describe "Multisig CBOR and hashes - golden tests" $ do

        let mnemonic = [ "test", "child", "burst", "immense", "armed", "parrot"
                       , "company", "walk", "dog" ]
        let (Right mw) = mkSomeMnemonic @'[9,12,15,18,21,24] mnemonic
        let sndFactor = mempty
        let rootK = genMasterKeyFromMnemonic mw sndFactor :: Shelley 'RootK XPrv
        let accXPrv = deriveAccountPrivateKey rootK minBound

        it "RequireSignatureOf index=0" $ do
            let index = minBound
            let multisigXPub = toXPub <$> deriveMultisigPrivateKey accXPrv index
            let verKeyHash@(VerificationKeyHash payload) = fromVerificationKey multisigXPub
            toHexText payload `shouldBe`
                "deeae4e895d8d57378125ed4fd540f9bf245d59f7936a504379cfc1e"
            let script = RequireSignatureOf verKeyHash
            TR.trace ("script raw bytes:"<>show (toScriptHash script)) $ (toHexText (toCBOR script) ) `shouldBe`
                "82008200581cdeeae4e895d8d57378125ed4fd540f9bf245d59f7936a504379cfc1e"
            (toHexText (toScriptHash script) )  `shouldBe`
                "59fd497a34ac3e5abf2c8a703e3aaf3a2750e207b139d65d08d2c1b3"
        it "RequireSignatureOf index=1" $ do
            let index = toEnum 0x00000001
            let multisigXPub = toXPub <$> deriveMultisigPrivateKey accXPrv index
            let verKeyHash@(VerificationKeyHash payload) = fromVerificationKey multisigXPub
            toHexText payload `shouldBe`
                "60a3bf69aa748f9934b64357d9f1ca202f1a768aaf57263aedca8d5f"
            let script = RequireSignatureOf verKeyHash
            (toHexText (toCBOR script) ) `shouldBe`
                "82008200581c60a3bf69aa748f9934b64357d9f1ca202f1a768aaf57263aedca8d5f"
        it "RequireSignatureOf index=2" $ do
            let index = toEnum 0x00000002
            let multisigXPub = toXPub <$> deriveMultisigPrivateKey accXPrv index
            let verKeyHash@(VerificationKeyHash payload) = fromVerificationKey multisigXPub
            toHexText payload `shouldBe`
                "ffcbb72393215007d9a0aa02b7430080409cd8c053fd4f5b4d905053"
            let script = RequireSignatureOf verKeyHash
            (toHexText (toCBOR script) ) `shouldBe`
                "82008200581cffcbb72393215007d9a0aa02b7430080409cd8c053fd4f5b4d905053"
        it "RequireSignatureOf index=3" $ do
            let index = toEnum 0x00000003
            let multisigXPub = toXPub <$> deriveMultisigPrivateKey accXPrv index
            let verKeyHash@(VerificationKeyHash payload) = fromVerificationKey multisigXPub
            toHexText payload `shouldBe`
                "96834025cdca063ce9c32dfae6bc6a3e47f8da07ee4fb8e1a3901559"
            let script = RequireSignatureOf verKeyHash
            (toHexText (toCBOR script) ) `shouldBe`
                "82008200581c96834025cdca063ce9c32dfae6bc6a3e47f8da07ee4fb8e1a3901559"

  where
    toHexText = T.decodeUtf8 . encode EBase16
