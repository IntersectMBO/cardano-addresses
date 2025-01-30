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
        specDetectEncoding (EBech32 ())
            "ed25519_sk1l25926aaaf7ty55g99r285wptc7nqrzager5jghurqdswklj4pvszzp8qg"

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
