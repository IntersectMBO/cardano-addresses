{-# LANGUAGE FlexibleContexts #-}

module Command.Key.PrivateSpec
    ( spec
    ) where

import Prelude

import Test.Hspec
    ( Spec, SpecWith, it, shouldBe, shouldContain )
import Test.Utils
    ( cli, describeCmd )

spec :: Spec
spec = describeCmd [ "key", "private" ] $ do
    specKeyNotPrivate

specKeyNotPrivate :: SpecWith ()
specKeyNotPrivate = it "fail if key isn't private" $ do
    (out, err) <- cli [ "recovery-phrase", "generate" ] ""
              >>= cli [ "key", "from-recovery-phrase", "icarus" ]
              >>= cli [ "key", "public", "--with-chain-code" ]
              >>= cli [ "key", "private", "--chain-code" ]

    out `shouldBe` ""
    err `shouldContain` "Invalid human-readable prefix."
