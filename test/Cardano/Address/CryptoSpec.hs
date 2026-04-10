{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Tests for 'Cardano.Address.Crypto' — covering every haddock claim.
module Cardano.Address.CryptoSpec
    ( spec
    ) where

import Prelude

import Cardano.Address.Crypto
    ( CryptoFailable (..)
    , DerivationScheme (..)
    , ScrubbedBytes
    , XPrv
    , XPub
    , blake2b160
    , blake2b224
    , blake2b256
    , ccDeriveXPrv
    , ccDeriveXPub
    , ccGenerate
    , ccGenerateNew
    , ccSign
    , ccToXPub
    , ccUnXPrv
    , ccVerify
    , ccXPrv
    , ccXPub
    , crc32
    , credentialHashSize
    , decryptChaChaPoly1305
    , ed25519ScalarMult
    , encryptChaChaPoly1305
    , hmacSha256
    , hmacSha512
    , pbkdf2HmacSha512
    , sha3_256
    )
import Cardano.Address.Derivation
    ( hashCredential, hashWalletId )
import Codec.Binary.Encoding
    ( AbstractEncoding (..), encode )
import Data.ByteString
    ( ByteString )
import Test.Hspec
    ( Spec, describe, it, shouldBe, shouldSatisfy )

import qualified Data.ByteArray as BA
import qualified Data.ByteArray.Encoding as BAE
import qualified Data.ByteString as BS

spec :: Spec
spec = describe "Cardano.Address.Crypto" $ do

    -- -----------------------------------------------------------
    -- Hashing
    -- -----------------------------------------------------------

    describe "blake2b160 (wallet ID hashing)" $ do
        it "produces 20 bytes" $
            BS.length (blake2b160 ("" :: ByteString)) `shouldBe` 20
        it "matches hashWalletId" $
            blake2b160 ("test" :: ByteString)
                `shouldBe` hashWalletId "test"
        it "known vector: empty" $
            toHex (blake2b160 ("" :: ByteString))
                `shouldBe`
                "3345524abf6bbe1809449224b5972c41790b6cf2"

    describe "blake2b224 (credential hashing)" $ do
        it "produces 28 bytes" $
            BS.length (blake2b224 ("" :: ByteString)) `shouldBe` 28
        it "credentialHashSize is 28" $
            credentialHashSize `shouldBe` 28
        it "matches hashCredential" $
            blake2b224 ("test" :: ByteString)
                `shouldBe` hashCredential "test"
        it "known vector: empty" $
            toHex (blake2b224 ("" :: ByteString))
                `shouldBe`
                "836cc68931c2e4e3e838602eca1902591d216837bafddfe6f0c8cb07"

    describe "blake2b256 (Byron seed hashing)" $ do
        it "produces 32 bytes" $
            BS.length (blake2b256 ("" :: ByteString)) `shouldBe` 32

    describe "sha3_256 (Byron address root)" $ do
        it "produces 32 bytes" $
            BS.length (sha3_256 ("" :: ByteString)) `shouldBe` 32
        it "blake2b224 . sha3_256 produces 28 bytes" $
            BS.length (blake2b224 (sha3_256 ("test" :: ByteString)))
                `shouldBe` 28

    -- -----------------------------------------------------------
    -- KDF
    -- -----------------------------------------------------------

    describe "pbkdf2HmacSha512" $ do
        it "Byron HD passphrase: 500 iterations, 32 bytes" $ do
            let result :: ScrubbedBytes
                result = pbkdf2HmacSha512 500 32
                    ("password" :: ByteString)
                    ("salt" :: ByteString)
            BA.length result `shouldBe` 32

        it "Icarus seed: 4096 iterations, 96 bytes" $ do
            let result :: ScrubbedBytes
                result = pbkdf2HmacSha512 4096 96
                    ("password" :: ByteString)
                    ("salt" :: ByteString)
            BA.length result `shouldBe` 96

        it "Ledger BIP39 seed: 2048 iterations, 64 bytes" $ do
            let result :: ScrubbedBytes
                result = pbkdf2HmacSha512 2048 64
                    ("password" :: ByteString)
                    ("mnemonic" :: ByteString)
            BA.length result `shouldBe` 64

    -- -----------------------------------------------------------
    -- HMAC
    -- -----------------------------------------------------------

    describe "hmacSha256 (Ledger chain code)" $ do
        it "produces 32 bytes" $
            BS.length (hmacSha256
                ("ed25519 seed" :: ByteString)
                ("data" :: ByteString))
                `shouldBe` 32

    describe "hmacSha512 (Ledger/SLIP-0010)" $ do
        it "produces 64 bytes" $
            BS.length (hmacSha512
                ("ed25519 seed" :: ByteString)
                ("data" :: ByteString))
                `shouldBe` 64

    -- -----------------------------------------------------------
    -- Symmetric encryption (Byron derivation paths)
    -- -----------------------------------------------------------

    describe "ChaChaPoly1305 (Byron derivation path encryption)" $ do
        it "encrypt then decrypt roundtrips" $ do
            let key = BS.replicate 32 0x42 :: ByteString
                nonce = "serokellfore" :: ByteString
                plaintext = "account-index:0,address-index:14"
            case encryptChaChaPoly1305 key nonce plaintext of
                CryptoFailed e ->
                    fail $ "encryption failed: " <> show e
                CryptoPassed ciphertext ->
                    case decryptChaChaPoly1305 key nonce ciphertext of
                        CryptoFailed e ->
                            fail $ "decryption failed: " <> show e
                        CryptoPassed result ->
                            result `shouldBe` plaintext

        it "cardano nonce 'serokellfore' is 12 bytes" $
            BS.length ("serokellfore" :: ByteString) `shouldBe` 12

        it "wrong key fails decryption" $ do
            let key1 = BS.replicate 32 0x42 :: ByteString
                key2 = BS.replicate 32 0x43 :: ByteString
                nonce = "serokellfore" :: ByteString
            case encryptChaChaPoly1305 key1 nonce "secret" of
                CryptoFailed _ -> fail "encryption failed"
                CryptoPassed ct ->
                    decryptChaChaPoly1305 key2 nonce ct
                        `shouldSatisfy` isCryptoFailed

    -- -----------------------------------------------------------
    -- Ed25519 (XPrv reconstruction)
    -- -----------------------------------------------------------

    describe "ed25519ScalarMult (XPrv reconstruction)" $ do
        it "RFC 8032 vector 1" $
            ed25519ScalarMult (fromHex
                "9d61b19deffd5a60ba844af492ec2cc4\
                \4449c5697b326919703bac031cae7f60")
                `shouldSatisfy` (/= Nothing)

        it "RFC 8032 vector 2" $
            ed25519ScalarMult (fromHex
                "4ccd089b28ff96da9db6c346ec114e0f\
                \5b8a319f35aba624da8cf6ed4fb8a6fb")
                `shouldSatisfy` (/= Nothing)

    -- -----------------------------------------------------------
    -- Extended keys (construction)
    -- -----------------------------------------------------------

    describe "ccXPrv / ccXPub" $ do
        it "ccXPrv rejects < 128 bytes" $
            isLeft' (ccXPrv (BS.replicate 64 0))
                `shouldBe` True

        it "ccXPub rejects < 64 bytes" $
            isLeft' (ccXPub (BS.replicate 32 0))
                `shouldBe` True

        it "ccXPub accepts 64 bytes" $
            isRight' (ccXPub (BS.replicate 64 0))
                `shouldBe` True

    -- -----------------------------------------------------------
    -- Key generation
    -- -----------------------------------------------------------

    describe "ccGenerate (Byron)" $ do
        it "produces 128-byte XPrv from >= 32 byte seed" $ do
            let seed = bsToScrubbed
                    "this is a seed that is at least 32 bytes long!!"
            BS.length (ccUnXPrv (ccGenerate seed mempty))
                `shouldBe` 128

    describe "ccGenerateNew (Icarus/Shelley)" $ do
        it "produces 128-byte XPrv" $ do
            let seed = bsToScrubbed "sixteen bytes!.."
            BS.length
                (ccUnXPrv (ccGenerateNew seed (mempty :: ScrubbedBytes) (mempty :: ScrubbedBytes)))
                `shouldBe` 128

    -- -----------------------------------------------------------
    -- Signing and verification
    -- -----------------------------------------------------------

    describe "ccSign / ccVerify" $ do
        it "sign then verify succeeds" $ do
            let (xprv, xpub) = testKeyPair
                sig = ccSign (mempty :: ScrubbedBytes) xprv
                    ("hello" :: ByteString)
            ccVerify xpub ("hello" :: ByteString) sig
                `shouldBe` True

        it "verify rejects wrong message" $ do
            let (xprv, xpub) = testKeyPair
                sig = ccSign (mempty :: ScrubbedBytes) xprv
                    ("hello" :: ByteString)
            ccVerify xpub ("wrong" :: ByteString) sig
                `shouldBe` False

    -- -----------------------------------------------------------
    -- Key derivation
    -- -----------------------------------------------------------

    describe "ccDeriveXPrv" $ do
        it "hardened derivation (>= 0x80000000) produces valid key" $ do
            let (root, _) = testKeyPair
                child = ccDeriveXPrv DerivationScheme2
                    (mempty :: ScrubbedBytes) root 0x80000000
            BS.length (ccUnXPrv child) `shouldBe` 128

        it "DerivationScheme1 vs DerivationScheme2 produce different keys" $ do
            let (root, _) = testKeyPair
                c1 = ccDeriveXPrv DerivationScheme1
                    (mempty :: ScrubbedBytes) root 0x80000000
                c2 = ccDeriveXPrv DerivationScheme2
                    (mempty :: ScrubbedBytes) root 0x80000000
            ccUnXPrv c1 `shouldSatisfy` (/= ccUnXPrv c2)

    describe "ccDeriveXPub" $ do
        it "soft derivation (< 0x80000000) succeeds" $ do
            let (_, xpub) = testKeyPair
            ccDeriveXPub DerivationScheme2 xpub 0
                `shouldSatisfy` (/= Nothing)

        it "hardened derivation returns Nothing" $ do
            let (_, xpub) = testKeyPair
            ccDeriveXPub DerivationScheme2 xpub 0x80000000
                `shouldBe` Nothing

    -- -----------------------------------------------------------
    -- CRC32 (Byron CBOR address checksum)
    -- -----------------------------------------------------------

    describe "crc32 (Byron address checksum)" $ do
        it "known vector: empty" $
            crc32 ("" :: ByteString) `shouldBe` 0
        it "known vector: 'test'" $
            crc32 ("test" :: ByteString) `shouldBe` 0xD87F7E0C

{-------------------------------------------------------------------------------
                                  Helpers
-------------------------------------------------------------------------------}

toHex :: ByteString -> ByteString
toHex = encode EBase16

fromHex :: ByteString -> ByteString
fromHex bs = case BAE.convertFromBase @ByteString BAE.Base16 bs of
    Right x -> x
    Left _ -> error "fromHex: invalid"

bsToScrubbed :: ByteString -> ScrubbedBytes
bsToScrubbed = BA.convert

testKeyPair :: (XPrv, XPub)
testKeyPair =
    let seed = bsToScrubbed
            "this is a seed that is at least 32 bytes long!!"
        xprv = ccGenerate seed (mempty :: ScrubbedBytes)
        xpub = ccToXPub xprv
     in (xprv, xpub)

isLeft' :: Either a b -> Bool
isLeft' (Left _) = True
isLeft' _ = False

isRight' :: Either a b -> Bool
isRight' (Right _) = True
isRight' _ = False

isCryptoFailed :: CryptoFailable a -> Bool
isCryptoFailed (CryptoFailed _) = True
isCryptoFailed _ = False
