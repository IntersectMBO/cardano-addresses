{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Cardano.AddressSpec
    ( spec
    ) where

import Prelude

import Cardano.Address
    ( Address
    , HasNetworkDiscriminant (..)
    , PaymentAddress (..)
    , base58
    , bech32
    , fromBase58
    , fromBech32
    , unAddress
    , unsafeMkAddress
    )
import Cardano.Address.Derivation
    ( Depth (..), XPub )
import Cardano.Address.Style.Byron
    ( Byron )
import Cardano.Address.Style.Icarus
    ( Icarus )
import Codec.Binary.Encoding
    ( AbstractEncoding (..), encode )
import Data.Function
    ( (&) )
import Data.Text
    ( Text )
import Data.Text.Encoding
    ( decodeLatin1 )
import Test.Arbitrary
    ()
import Test.Hspec
    ( Spec, describe, it, shouldBe )
import Test.Hspec.QuickCheck
    ( prop )
import Test.QuickCheck
    ( Property, counterexample, label, (===) )

import qualified Data.ByteString as BS
import qualified Data.Text as T

spec :: Spec
spec = describe "Text Encoding Roundtrips" $ do
    prop "base58 . fromBase58 - Byron" $
        prop_roundtripTextEncoding @Byron base58 fromBase58

    prop "base58 . fromBase58 - Icarus" $
        prop_roundtripTextEncoding @Icarus base58 fromBase58

    prop "bech32 . fromBech32 - Byron" $
        prop_roundtripTextEncoding @Byron bech32 fromBech32

    prop "bech32 . fromBech32 - Icarus" $
        prop_roundtripTextEncoding @Icarus bech32 fromBech32

    prop "fromBase58 rejects trailing bytes - Byron" $
        prop_rejectTrailingBytesFromBase58 @Byron

    prop "fromBase58 rejects trailing bytes - Icarus" $
        prop_rejectTrailingBytesFromBase58 @Icarus

    -- CVE-2025-XXXX regression controls:
    -- Valid legacy addresses must parse, but mutated variants with trailing
    -- bytes appended before Base58 re-encoding must be rejected.
    it "fromBase58 accepts valid Icarus control" $ do
        let Just addr = fromBase58 "Ae2tdPwUPEYz6ExfbWubiXPB6daUuhJxikMEb4eXRp5oKZBKZwrbJ2k7EZe"
        base58 addr `shouldBe` "Ae2tdPwUPEYz6ExfbWubiXPB6daUuhJxikMEb4eXRp5oKZBKZwrbJ2k7EZe"

    it "fromBase58 rejects mutated Icarus control" $ do
        fromBase58 "FHnt4NHDYS3mWHPRvgbJio3nHma3qW76bo7z9oYfDsie9GpqdUT5VFrQrVkhie5"
            `shouldBe` Nothing

    it "fromBase58 accepts valid Byron control" $ do
        let Just addr = fromBase58 "DdzFFzCqrht5csm2GKhnVrjzKpVHHQFNXUDhAFDyLWVY5w8ZsJRP2uhwZq2CEAVzDZXYXa4GvggqYEegQsdKAKikFfrrCoHheLH2Jskr"
        base58 addr `shouldBe` "DdzFFzCqrht5csm2GKhnVrjzKpVHHQFNXUDhAFDyLWVY5w8ZsJRP2uhwZq2CEAVzDZXYXa4GvggqYEegQsdKAKikFfrrCoHheLH2Jskr"

    it "fromBase58 rejects mutated Byron control" $ do
        fromBase58 "KjgoiXEdBrKbPJXtVLzpChsXvvj5FDS3jnjBLuPPRSQcTpbNEme4QBbvQCopCDNKVunvXRibDdSgk9pzKpX9Vz8QnyhCsoCBpDujrYqXaRpD"
            `shouldBe` Nothing

-- Ensure that any address public key can be encoded to an address and that the
-- address can be encoded and decoded without issues.
prop_roundtripTextEncoding
    :: forall k. (PaymentAddress k)
    => (Address -> Text)
        -- ^ encode to 'Text'
    -> (Text -> Maybe Address)
        -- ^ decode from 'Text'
    -> k 'PaymentK XPub
        -- ^ An arbitrary public key
    -> NetworkDiscriminant k
        -- ^ An arbitrary network discriminant
    -> Property
prop_roundtripTextEncoding encode decode addXPub discrimination =
    (result == pure address)
        & counterexample (unlines
            [ "Address " <> T.unpack (encode address)
            , "↳       " <> maybe "ø" (T.unpack . encode) result
            ])
        & label (show $ addressDiscrimination @k discrimination)
  where
    address = paymentAddress discrimination addXPub
    result  = decode (encode address)

-- | Appending trailing bytes to a valid address and re-encoding in base58
-- should be rejected by 'fromBase58'.
prop_rejectTrailingBytesFromBase58
    :: forall k. (PaymentAddress k)
    => k 'PaymentK XPub
    -> NetworkDiscriminant k
    -> Property
prop_rejectTrailingBytesFromBase58 addXPub discrimination =
    fromBase58 malformedText === Nothing
  where
    address = paymentAddress discrimination addXPub
    malformedBytes = unAddress address <> BS.singleton 0
    malformedText = decodeLatin1 $ encode EBase58 malformedBytes
