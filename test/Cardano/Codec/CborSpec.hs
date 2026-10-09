{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Cardano.Codec.CborSpec
    ( spec
    ) where

import Prelude

import Cardano.Address.Crypto
    ( crc32 )
import Cardano.Address.Derivation
    ( Depth (..), GenMasterKey (..), XPrv )
import Cardano.Address.Style.Byron
    ( Byron (..) )
import Cardano.Codec.Cbor
    ( decodeAddress
    , decodeAddressDerivationPath
    , decodeAddressPayload
    , decodeAllAttributes
    , decodeDerivationPathAttr
    , deserialiseCbor
    , encodeAttributes
    , encodeDerivationPathAttr
    , toLazyByteString
    , toStrictByteString
    , unsafeDeserialiseCbor
    )
import Cardano.Mnemonic
    ( mkSomeMnemonic )
import Control.Monad
    ( forM_ )
import Data.ByteArray
    ( ByteArrayAccess, ScrubbedBytes )
import Data.ByteString
    ( ByteString )
import Data.List
    ( isInfixOf )
import Data.Text
    ( Text )
import Data.Word
    ( Word, Word32, Word8 )
import Test.Arbitrary
    ( unsafeFromHex )
import Test.Hspec
    ( Expectation, Spec, describe, it, shouldBe, shouldSatisfy )
import Test.QuickCheck
    ( Arbitrary (..), Property, conjoin, property, vector, (===), (==>) )

import Cardano.Address.Internal
    ( DeserialiseFailure (..) )
import qualified Codec.CBOR.Encoding as CBOR
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

{- HLINT ignore "Use head" -}

spec :: Spec
spec = do
    describe "decodeAddress <-> encodeAddress roundtrip" $ do
        it "DerivationPath roundtrip" (property prop_derivationPathRoundTrip)

    describe "Golden Tests for Byron Addresses w/ random scheme (Mainnet)" $ do
        it "decodeDerivationPath - mainnet - initial account" $
            decodeDerivationPathTest DecodeDerivationPath
                { mnem =
                    arbitraryMnemonic
                , addr =
                    "82d818584283581ca08bcb9e5e8cd30d5aea6d434c46abd8604fe4907d\
                    \56b9730ca28ce5a101581e581c22e25f2464ec7295b556d86d0ec33bc1\
                    \a681e7656da92dbc0582f5e4001a3abe2aa5"
                , accIndex =
                    2147483648
                , addrIndex =
                    2147483648
                }

        it "decodeDerivationPath - mainnet - another account" $
            decodeDerivationPathTest DecodeDerivationPath
                { mnem =
                    arbitraryMnemonic
                , addr =
                    "82d818584283581cb039e80866203e82fc834b8e6a355b83ec6f8fd199\
                    \66078a40e6d6b2a101581e581c22e27fb12d08728073cd416dfbfcb8dc\
                    \0e760335d1d60f65e8740034001a4bce4d1a"
                , accIndex =
                    2694138340
                , addrIndex =
                    2512821145
                }

    describe "Golden Tests for Byron Addresses w/ random scheme (Testnet)" $ do
        it "decodeDerivationPath - testnet - initial account" $
            decodeDerivationPathTest DecodeDerivationPath
                { mnem =
                    arbitraryMnemonic
                , addr =
                    "82d818584983581ca03d42af673855aabcef3059e21c37235ae706072d\
                    \38150dcefae9c6a201581e581c22e25f2464ec7295b556d86d0ec33bc1\
                    \a681e7656da92dbc0582f5e402451a4170cb17001a39a0b7b5"
                , accIndex =
                    2147483648
                , addrIndex =
                    2147483648
                }

        it "decodeDerivationPath - testnet - another account" $
            decodeDerivationPathTest DecodeDerivationPath
                { mnem =
                    arbitraryMnemonic
                , addr =
                    "82d818584983581c267b40902921c3afd73926a83a23ca08ae9626a64a\
                    \4b5616d14d6709a201581e581c22e219c90fb572d565134f6daeab650d\
                    \c871d130430afe594116f1ae02451a4170cb17001aee75f28a"
                , accIndex =
                    3337448281
                , addrIndex =
                    3234874775
                }

    describe "Trailing bytes are rejected" $ do
        it "deserialiseCbor rejects trailing bytes" $ do
            let addr = unsafeFromHex
                    "82d818584283581ca08bcb9e5e8cd30d5aea6d434c46abd8604fe4907d\
                    \56b9730ca28ce5a101581e581c22e25f2464ec7295b556d86d0ec33bc1\
                    \a681e7656da92dbc0582f5e4001a3abe2aa5"
            let malformed = addr <> "\NUL"
            deserialiseCbor decodeAddressPayload malformed
                `shouldBe` Left
                    (DeserialiseFailure 0 "Leftovers when decoding CBOR")

    describe "Byron address CBOR tag validation" $ do
        it "accepts the canonical tag 24 wrapper" $ do
            deserialiseCbor decodeAddress byronAddressFixture
                `shouldBe` Right byronAddressFixture

        it "rejects any tag other than 24, payload and CRC intact" $ do
            forM_
                [ 0, 1, 23, 25, 42, 1000, 65535, 65536, 4294967296, maxBound ]
                $ \tag -> do
                    let malformed = rewrapTag tag byronAddressFixture
                    deserialiseCbor decodeAddress malformed
                        `shouldSatisfy` isTagError
                    deserialiseCbor decodeAddressPayload malformed
                        `shouldSatisfy` isTagError

        it "still enforces the crc32 check on tag-24 wrappers" $ do
            let malformed = BS.init byronAddressFixture <> "\NUL"
            deserialiseCbor decodeAddress malformed
                `shouldSatisfy` isCrcError
            deserialiseCbor decodeAddressPayload malformed
                `shouldSatisfy` isCrcError

        it "rejects any non-24 tag on an otherwise valid wrapper (property)" $
            property prop_wrapperTagValidation

        it "accepts tag 24 on an otherwise valid wrapper (property)" $
            property prop_wrapperTag24Decodes


{-------------------------------------------------------------------------------
                    Golden tests for Address derivation path
-------------------------------------------------------------------------------}

-- | A structurally valid Byron address (mainnet, random scheme). Its CBOR
-- wrapper is @[24, payload, crc32(payload)]@ with the tag encoded as the two
-- bytes @d818@ right after the leading @82@ (list of two).
byronAddressFixture :: ByteString
byronAddressFixture = unsafeFromHex
    "82d818584283581ca08bcb9e5e8cd30d5aea6d434c46abd8604fe4907d\
    \56b9730ca28ce5a101581e581c22e25f2464ec7295b556d86d0ec33bc1\
    \a681e7656da92dbc0582f5e4001a3abe2aa5"

-- | Rewrite the CBOR tag of a Byron address wrapper, preserving its payload
-- and CRC. Only valid when the wrapper carries tag 24 (@d818@) at bytes 1-2.
rewrapTag :: Word -> ByteString -> ByteString
rewrapTag tag bytes = BS.take 1 bytes <> encodedTag <> BS.drop 3 bytes
  where
    encodedTag = toStrictByteString $ CBOR.encodeTag tag

-- | Encode an arbitrary payload as a Byron address wrapper with the given
-- tag, mirroring 'Cardano.Codec.Cbor.encodeAddressPayload'.
byronWrapper :: Word -> ByteString -> ByteString
byronWrapper tag payload = toStrictByteString $ mempty
    <> CBOR.encodeListLen 2
    <> CBOR.encodeTag tag
    <> CBOR.encodeBytes payload
    <> CBOR.encodeWord32 (crc32 payload)

isTagError :: Either DeserialiseFailure a -> Bool
isTagError result = case result of
    Left (DeserialiseFailure _ msg) -> "unexpected CBOR tag" `isInfixOf` msg
    Right _ -> False

isCrcError :: Either DeserialiseFailure a -> Bool
isCrcError result = case result of
    Left (DeserialiseFailure _ msg) -> "non-matching crc32" `isInfixOf` msg
    Right _ -> False

-- | A tag other than 24 must be rejected by both decoders, no matter which
-- payload and CRC the wrapper carries.
prop_wrapperTagValidation :: [Word8] -> Word -> Property
prop_wrapperTagValidation payloadBytes tag =
    conjoin
        [ property $ isTagError (deserialiseCbor decodeAddressPayload wrapper)
        , property $ isTagError (deserialiseCbor decodeAddress wrapper)
        ]
  where
    tag' = if tag == 24 then 25 else tag
    wrapper = byronWrapper tag' (BS.pack payloadBytes)

-- | Wrappers carrying tag 24 and a valid CRC must be accepted by both
-- decoders.
prop_wrapperTag24Decodes :: [Word8] -> Property
prop_wrapperTag24Decodes payloadBytes =
    conjoin
        [ deserialiseCbor decodeAddressPayload wrapper === Right payload
        , deserialiseCbor decodeAddress wrapper === Right wrapper
        ]
  where
    payload = BS.pack payloadBytes
    wrapper = byronWrapper 24 payload

data DecodeDerivationPath = DecodeDerivationPath
    { mnem :: [Text]
    , addr :: ByteString
    , accIndex :: Word32
    , addrIndex :: Word32
    } deriving (Show, Eq)

-- An aribtrary mnemonic sentence for the tests
arbitraryMnemonic :: [Text]
arbitraryMnemonic =
    [ "price", "whip", "bottom", "execute", "resist", "library"
    , "entire", "purse", "assist", "clock", "still", "noble" ]

decodeDerivationPathTest :: DecodeDerivationPath -> Expectation
decodeDerivationPathTest DecodeDerivationPath{..} =
    decoded `shouldBe` Right (Just (accIndex, addrIndex))
  where
    payload = unsafeDeserialiseCbor decodeAddressPayload $
        BL.fromStrict (unsafeFromHex addr)
    decoded = deserialiseCbor (decodeAddressDerivationPath pwd) payload
    Right mw = mkSomeMnemonic @'[12] mnem
    key = genMasterKeyFromMnemonic mw () :: Byron 'RootK XPrv
    pwd = payloadPassphrase key

{-------------------------------------------------------------------------------
                           Derivation Path Roundtrip
-------------------------------------------------------------------------------}

prop_derivationPathRoundTrip
    :: Passphrase
    -> Passphrase
    -> Word32
    -> Word32
    -> Property
prop_derivationPathRoundTrip (Passphrase pwd) (Passphrase pwd') acctIx addrIx =
    let
        encoded = toLazyByteString $ encodeAttributes
            [ encodeDerivationPathAttr pwd acctIx addrIx ]
        decoded = unsafeDeserialiseCbor
                (decodeAllAttributes >>=  decodeDerivationPathAttr pwd)
                encoded
        decoded' = unsafeDeserialiseCbor
                (decodeAllAttributes >>=  decodeDerivationPathAttr pwd')
                encoded
    in
        conjoin
            [ decoded === Just (acctIx, addrIx)
            , pwd /= pwd' ==> decoded' === Nothing
            ]

{-------------------------------------------------------------------------------
                             Arbitrary Instances
-------------------------------------------------------------------------------}

newtype Passphrase = Passphrase ScrubbedBytes
    deriving stock (Eq, Show)
    deriving newtype (ByteArrayAccess)

instance Arbitrary Passphrase where
    arbitrary = do
        bytes <- BS.pack <$> vector 32
        return $ Passphrase $ BA.convert bytes
