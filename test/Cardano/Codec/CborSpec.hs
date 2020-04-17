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

module Cardano.Codec.CborSpec
    ( spec
    ) where

import Prelude

import Cardano.Address.Derivation
    ( Depth (..), DerivationType (..), Index (..) )
import Cardano.Address.Style.Byron
    ( Byron (..), unsafeGenerateKeyFromSeed )
import Cardano.Codec.Cbor
    ( decodeAddressDerivationPath
    , decodeAddressPayload
    , decodeAllAttributes
    , decodeDerivationPathAttr
    , deserialiseCbor
    , encodeAttributes
    , encodeDerivationPathAttr
    , toLazyByteString
    , unsafeDeserialiseCbor
    )
import Cardano.Crypto.Wallet
    ( XPrv )
import Cardano.Mnemonic
    ( mkSomeMnemonic )
import Data.ByteArray
    ( ByteArrayAccess, ScrubbedBytes )
import Data.ByteString
    ( ByteString )
import Data.Text
    ( Text )
import Data.Word
    ( Word32 )
import Test.Arbitrary
    ( unsafeFromHex )
import Test.Hspec
    ( Expectation, Spec, describe, it, shouldBe )
import Test.QuickCheck
    ( Arbitrary (..), Property, conjoin, property, vector, (===), (==>) )

import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

{-# ANN spec ("HLint: ignore Use head" :: String) #-}

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

{-------------------------------------------------------------------------------
                    Golden tests for Address derivation path
-------------------------------------------------------------------------------}

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
    decoded `shouldBe` Just (Just (Index accIndex, Index addrIndex))
  where
    payload = unsafeDeserialiseCbor decodeAddressPayload $
        BL.fromStrict (unsafeFromHex addr)
    decoded = deserialiseCbor (decodeAddressDerivationPath pwd) payload
    Right seed = mkSomeMnemonic @'[12] mnem
    key = unsafeGenerateKeyFromSeed () seed mempty :: Byron 'RootK XPrv
    pwd = payloadPassphrase key

{-------------------------------------------------------------------------------
                           Derivation Path Roundtrip
-------------------------------------------------------------------------------}

prop_derivationPathRoundTrip
    :: Passphrase
    -> Passphrase
    -> Index 'WholeDomain 'AccountK
    -> Index 'WholeDomain 'AddressK
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
