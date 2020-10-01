{-# LANGUAGE FlexibleContexts #-}

module Command.Key.HashSpec
    ( spec
    ) where

import Prelude

import Test.Hspec
    ( Spec, SpecWith, it, shouldBe, shouldContain )
import Test.Utils
    ( cli, describeCmd )

spec :: Spec
spec = describeCmd [ "key", "hash" ] $ do
    specKeyNotPublic

specKeyNotPublic :: SpecWith ()
specKeyNotPublic = it "fail if key isn't public" $ do
    (out, err) <- cli [ "recovery-phrase", "generate" ] ""
              >>= cli [ "key", "from-recovery-phrase", "icarus" ]
              >>= cli [ "key", "hash" ]

    out `shouldBe` ""
    err `shouldContain` "Couldn't convert bytes into extended public key."
