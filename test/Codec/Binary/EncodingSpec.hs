{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Codec.Binary.EncodingSpec
    ( spec
    ) where

import Prelude

import Codec.Binary.Encoding
    ( AbstractEncoding (..), detectEncoding )
import Test.Hspec
    ( Spec, SpecWith, describe, it, shouldBe )

spec :: Spec
spec = do
    describe "detectEncoding" $ do
        -- Base16 detection must remain unchanged.
        specDetectEncoding EBase16
            "c37e1b501e1821c581130d573812fa67f56efabc"

        -- Base58 detection must remain unchanged, including for strings whose
        -- characters are also part of the Bech32 data charset but that contain
        -- no '1' separator.
        specDetectEncoding EBase58
            "qpzry9x8gf2tvdw"
        specDetectEncoding EBase58
            "Ae2tdPwUPEZ5QJkfzoJgarugsX3rUVbTjg8nqTYmuy2c2msy5augpnm91ZR"
        specDetectEncoding EBase58
            "DdzFFzCqrht5csm2GKhnVrjzKpVHHQFNXUDhAFDyLWVY5w8ZsJRP2uhwZq2CEAVzDZXYXa4GvggqYEegQsdKAKikFfrrCoHheLH2Jskr"

        -- Bech32 detection must work for both lowercase and uppercase strings.
        specDetectEncoding (EBech32 ())
            "ed25519_sk1l25926aaaf7ty55g99r285wptc7nqrzager5jghurqdswklj4pvszzp8qg"
        specDetectEncoding (EBech32 ())
            "ADDR1VPU5VLRF4XKXV2QPWNGF6CJHTW542AYTY80V8DYR49RF5EG0YU80W"
        specDetectEncoding (EBech32 ())
            "addr1vpu5vlrf4xkxv2qpwngf6cjhtw542ayty80v8dyr49rf5eg0yu80w"

        -- Bech32 human-readable parts may themselves contain '1' characters; the
        -- separator is the *last* '1' in the string (BIP-0173).
        specDetectEncoding (EBech32 ())
            "cip1852_vk1axaxssg2p4e8x6v24nz4jnv9ntqctccpd8r4pqch9kmjv5t6gj2c5q6ks"

        -- Invalid / ambiguous inputs must not be detected as Bech32.
        -- Contains only Bech32 data characters but no separator and is not
        -- Base58 (because of '0'), so it must not be misclassified as Bech32.
        specDetectEncodingNothing "qpzry0"
        -- Empty HRP and not Base58 (because of '0'); must not be Bech32.
        specDetectEncodingNothing "1qpzry90"
        -- Data part too short and not Base58 (because of 'O').
        specDetectEncodingNothing "a1SHORT"
        -- Mixed case must be rejected.
        specDetectEncodingNothing "Addr1VPU5VLRF4XKXV2QPWNGF6CJHTW542AYTY80V8DYR49RF5EG0YU80W"
        -- Uppercase data part contains 'O' which is not a valid Bech32 data
        -- character, so this cannot be Bech32.
        specDetectEncodingNothing "ADDR_VK17AXH4SC9ZWKPSFT3TLGPJEMFWCOU5MNLD80R85ZW7ZDQCST6W54SDV4A4E"

specDetectEncoding
    :: AbstractEncoding ()
    -> String
    -> SpecWith ()
specDetectEncoding expected str =
    it (str <> " is " <> pretty expected) $
        detectEncoding str `shouldBe` Just expected
  where
    pretty = \case
        EBech32{} -> "bech32"
        EBase16{} -> "base16"
        EBase58{} -> "base58"

specDetectEncodingNothing
    :: String
    -> SpecWith ()
specDetectEncodingNothing str =
    it (str <> " has no detectable encoding") $
        detectEncoding str `shouldBe` Nothing
